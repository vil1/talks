import code.Diff
import shapeless._

trait Delta[R <: HList] extends DepFn2[R, R] {
  type Out <: HList
}