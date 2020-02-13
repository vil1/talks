import shapeless._
import shapeless.labelled.FieldType

import scala.reflect.ClassTag

/**
  * @author Valentin Kasas
  */
trait PPrint[R] extends DepFn0 {
  type Out = String
}

trait LowPriorityPPrint {

  implicit def hconsPPrint[H, T <: HList]
  (implicit ct: ClassTag[H],
   tailpprint: PPrint[T]) = new PPrint[H :: T] {
    override def apply(): String = ct.toString() + " :: " + tailpprint.apply()
  }

  implicit def cconsPPrint[H, T <: Coproduct]
  (implicit ct: ClassTag[H],
   tailpprint: PPrint[T]) = new PPrint[H :+: T] {
    override def apply(): String = ct.toString() + " :+: " + tailpprint.apply()
  }
}


object PPrint extends LowPriorityPPrint{
  implicit def hnilPPrint = new PPrint[HNil] {
    override def apply(): String = "HNil"
  }

  implicit def cnilPPrint = new PPrint[CNil] {
    override def apply(): String = "CNil"
  }

  implicit def hconsLabelledPPrint[K <: Symbol, V, T <: HList]
  (implicit cth: ClassTag[V],
   ctk: Witness.Aux[K],
   tailpprint: PPrint[T]) = new PPrint[FieldType[K, V] :: T] {
    override def apply(): String = "'"+ ctk.value.name + " ->> " + cth.toString() + " :: " + tailpprint.apply()
  }

  implicit def cconsLabelledPPrint[K <: Symbol, V, T <: Coproduct]
  (implicit cth: ClassTag[V],
   ctk: Witness.Aux[K],
   tailpprint: PPrint[T]) = new PPrint[FieldType[K, V]:+: T] {
    override def apply(): String = "'" + ctk.value.name + " ->> " + cth.toString() + " :+: " + tailpprint.apply()
  }
}

object PrettyPrinter {

  def prettyPrintGeneric[A, R](gen: Generic.Aux[A, R])(implicit pprint: PPrint[R]): String = pprint.apply()
  def prettyPrintLabelledGeneric[A, R](gen: LabelledGeneric.Aux[A, R])(implicit pprint: PPrint[R]) = pprint.apply()

}


sealed trait Base
case class Foo(long: Long, string: String, boolean: Boolean) extends Base
case class Bar(int: Int, double: Double) extends Base

object ppdemo {

  import PrettyPrinter._

  val foo = prettyPrintGeneric(Generic[Foo]) // ~> Long :: java.lang.String :: Boolean :: HNil
  val base = prettyPrintLabelledGeneric(LabelledGeneric[Base]) // ~> 'Foo ->> Foo :+: 'Bar ->> Bar :+: CNil
}