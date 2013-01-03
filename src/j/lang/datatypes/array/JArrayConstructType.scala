package j.lang.datatypes.array

trait JArrayConstructType[T]

object JArrayConstructType {
  implicit object JCInt extends JArrayConstructType[Int]
  implicit object JCDouble extends JArrayConstructType[Double]
  implicit object JCChar extends JArrayConstructType[Char]
  implicit object JCBox  extends JArrayConstructType[JArray[_]]
}