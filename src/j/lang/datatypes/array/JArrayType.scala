package j.lang.datatypes.array

import j.util.CMacroType
import j.util.Rational
import scala.math.Numeric$

sealed trait JArrayType[InternalType]

object JArrayType {

	  implicit object JB01 		extends JArrayType[Byte]
	  implicit object JINT 		extends JArrayType[Int]
	  implicit object JFlOAT 	extends JArrayType[Double]
	  implicit object JCHAR		extends JArrayType[Char]
	  implicit object JXNUM		extends JArrayType[Rational]
	  
	  class Box[T: JArrayType](val value: T)
	  implicit object JBOX	extends JArrayType[Box[_]]
	  
	  trait JNUMERIC[T] extends scala.math.Numeric[T]
}