/**
  * @author Valentin Kasas
  */

import shapeless._


object finder {

  trait Finder[L <: HList, P <: Poly1] extends DepFn2[L, P] {type Out}

  object Finder {

    def apply[L <: HList, P <: Poly1](implicit f: Finder[L, P]): Aux[L, P, f.Out] = f

    type Aux[L <: HList, P <: Poly1, O] = Finder[L, P]{type Out = O}

    implicit def hnilFinder[P <: Poly1]: Aux[HNil, P, Option[Nothing]] = new Finder[HNil, P]{

      override type Out = Option[Nothing]

      override def apply(t: HNil, u: P): Out = None
    }

    implicit def hconsFinder[H, T <: HList, P <: Poly1]
    (implicit
      appl: poly.Case1.Aux[P, H, Boolean],
      tf: Finder[T, P]
    ): Finder[H::T, P] = new Finder[H :: T, P]{
      type Out = Option[H] :+: tf.Out :+: CNil

      def apply(list: H :: T, pred: P) = if(appl(list.head)){ Inl(Option(list.head))} else Inr(Inl(tf(list.tail, pred)))
    }

  }

  def find[L <: HList, P <: Poly1](list: L, pred: P)(implicit finder: Finder[L, P]): finder.Out = finder(list, pred)

  case class X[A](id: String, a: A)

  object Predicate extends Poly1 {
    implicit def atX[A]: Case.Aux[X[A], Boolean] = at[X[A]](x => x.id == "foo")
  }

}
