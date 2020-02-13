package code
import shapeless._
import shapeless.ops.coproduct.{Mapper => _}
import shapeless.ops.record.{Selector, Updater}
/**
  * @author Valentin Kasas
  */

sealed trait Diff[A]
final case class Different[A](left: A, right: A) extends Diff[A]
final case class Identical[A](value: A) extends Diff[A]

object Diff {
  def apply[A](l: A, r: A): Diff[A] = if (l == r) Identical(l) else Different(l, r)
}


trait Delta[R <: HList] extends DepFn2[R, R]{
  type Out <: HList
}

trait LowPriorityDelta {
  implicit def hconsNestedDelta[H, T <: HList, DT <: HList]
  (implicit tailDelta: Delta.Aux[T, DT]) : Delta.Aux[H :: T, Diff[H] :: DT] =
    new Delta[H :: T] {
      override type Out = Diff[H] :: DT

      override def apply(l: H :: T, r: H :: T): Out =
        Diff(l.head, r.head) :: tailDelta(l.tail, r.tail)
    }
}

object Delta extends LowPriorityDelta {

  type Aux[R <: HList, O <: HList] = Delta[R]{type Out = O}

  def apply[A, R <: HList, O <: HList](l: A, r: A)
                                      (implicit genA: Generic.Aux[A, R], nestedDelta: Delta[R]): nestedDelta.Out =
    nestedDelta(genA.to(l), genA.to(r))


  implicit def hnilNestedDelta: Aux[HNil, HNil] = new Delta[HNil] {
    override type Out = HNil

    override def apply(l: HNil, r: HNil): Out = HNil
  }

  implicit def hconsGenNestedDelta[H, GH <: HList, DH <: HList,  T <: HList, DT <: HList]
  (implicit
   genH: Generic.Aux[H, GH],
   nested: Delta.Aux[GH, DH],
   tailDelta: Delta.Aux[T, DT]
  ): Aux[H :: T, DH :: DT] =
    new Delta[H :: T] {
      override type Out = DH :: DT

      override def apply(l: H :: T, r: H :: T): Out =
        nested(genH.to(l.head), genH.to(r.head)) :: tailDelta(l.tail, r.tail)
    }
}


trait Patcher[R <: HList, P <: HList] {
  def apply(repr: R, patch: P): R
}

trait LowPriorityPatcher {

  implicit def hconsPatcher[H, T <: HList, PT <: HList]
  (implicit tailPatcher: Patcher[T, PT]): Patcher[H::T, Diff[H]::PT] = new Patcher[H::T, Diff[H]::PT] {
    override def apply(repr: H :: T, patch: Diff[H] :: PT): H :: T = {

      val head = patch.head match {
        case Identical(_) => repr.head
        case Different(_, x) => x
      }
      head :: tailPatcher(repr.tail, patch.tail)
    }
  }
}


object Patcher extends LowPriorityPatcher{

  def apply[A, R <: HList, P <: HList](value: A, patch: P)
  (implicit gen: Generic.Aux[A, R], patcher: Patcher[R, P]): A =
    gen.from(patcher(gen.to(value), patch))

  implicit def hnilPatcher: Patcher[HNil, HNil] = new Patcher[HNil, HNil] {
    override def apply(repr: HNil, patch: HNil): HNil = HNil
  }

  implicit def hconsGenPatcher[H, GH <: HList, T <: HList, PH <: HList, PT <: HList]
  (implicit genH: Generic.Aux[H, GH],
    headPatcher: Patcher[GH, PH],
    tailPatcher: Patcher[T, PT])
  : Patcher[H::T, PH::PT] =
    new Patcher[H::T, PH::PT] {
      override def apply(repr: H :: T, patch: PH :: PT): H :: T =
        genH.from(headPatcher(genH.to(repr.head), patch.head)) :: tailPatcher(repr.tail, patch.tail)
    }
}


trait SimpleDelta[R <: HList] extends DepFn2[R, R] {
  type Out <: HList
}

object SimpleDelta {

  type Aux[I <: HList, O <: HList] = SimpleDelta[I]{ type Out = O }

  def apply[A, R <: HList, O <: HList](l: A, r: A)
    (implicit genA: Generic.Aux[A, R], delta: SimpleDelta.Aux[R, O])
  : O = delta(genA.to(l), genA.to(r))

  implicit def hnilDelta: Aux[HNil, HNil] = new SimpleDelta[HNil] {

    type Out = HNil

    def apply(l: HNil, r: HNil): Out = HNil

  }

  implicit def hconsDelta[H, T <: HList, DT <: HList]
  (implicit tailDelta: Aux[T, DT]): Aux[H::T, Diff[H] :: DT] = new SimpleDelta[H :: T]{

    type Out = Diff[H] :: DT

    def apply(l: H :: T, r: H :: T) : Out =
      Diff(l.head, r.head) :: tailDelta(l.tail, r.tail)
  }

}
