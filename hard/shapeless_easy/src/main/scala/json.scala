import shapeless._
import shapeless.labelled._

trait LowerPriorityToJson extends Poly1 {
  type ==>[In, Out] = Case.Aux[In, Out]
  implicit def anyCase[T]: T ==> String = at[T]("\"" + _.toString + "\"")

}
trait LowPriorityToJson extends LowerPriorityToJson {

  implicit def productCase[A <: Product, R <: HList]
  (implicit gen: LabelledGeneric.Aux[A, R],
   fieldsToJson: R ==> List[String])
  : A ==> String =
    at[A](a => "{" + fieldsToJson(gen.to(a)).mkString(", ") + "}")

  implicit def hnilCase : HNil ==> List[String] =
    at[HNil](_ => List.empty[String])

  implicit def hconsCase[K <: Symbol, H, T <: HList]
  (implicit w: Witness.Aux[K],
   headToJson: H ==> String,
   tailToJson: T ==> List[String])
  : (FieldType[K, H] :: T) ==> List[String] =
    at[FieldType[K, H] :: T](fields =>
      s""""${w.value.name}": ${headToJson(fields.head.asInstanceOf[H])}"""
        :: tailToJson(fields.tail)
    )

}
object ToJson extends LowPriorityToJson {

  implicit def numericCase[N : Numeric]: N ==> String = at[N](_.toString)

  implicit def booleanCase: Boolean ==> String = at[Boolean](_.toString)

  implicit def stringCase: String ==> String = at[String]("\"" + _ + "\"")

  implicit def listCase[A]
  (implicit elemToJson: A ==> String)
  : List[A] ==> String =
    at[List[A]](l => "[" + (l map elemToJson.apply[A] mkString ", ") + "]")

  implicit def optionCase[A]
  (implicit elemToJson: A ==> String)
  : Option[A] ==> String =
    at[Option[A]](_.fold("null")(elemToJson.apply[A]))

}