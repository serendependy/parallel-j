package j.lang.datatypes.array

import j.util.CMacroType
import j.util.Rational
import scala.math.{Numeric, Numeric$, Integral, Integral$, Fractional, Fractional$}
import scala.math.Numeric._

sealed class JArrayType[T]

object JArrayType {

	  //the three type families in J
	  implicit object JCHAR		extends JArrayType[Char]

	  class Box[T: JArrayType](val value: T)
	  implicit object JBOX	extends JArrayType[Box[_]]
	  
	  trait JNUMERIC[T]		extends JArrayType[T]	with Numeric[T] {
	    
	  }
	  
	  // the two branches of the numeric type family in J
	  trait JINTEGRAL[T]	extends JNUMERIC[T]		with Integral[T]
	  trait JFRACTIONAL[T]	extends JNUMERIC[T]		with Fractional[T]

	  trait JB01		extends JINTEGRAL[Byte]
	  trait JINT		extends JINTEGRAL[Int]
	  trait JFLOAT		extends JFRACTIONAL[Double]
	  trait JXNUM		extends JFRACTIONAL[Rational]
	  
	  // the integral branch
	  implicit object JB01IsJINTEGRAL extends JB01	with ByteIsIntegral
	  implicit object JINTIsJINTEGRAL extends JINT	with IntIsIntegral

	  // the fractional branch
	  implicit object JFLOATIsJFRACTIONAL extends JFLOAT with DoubleIsFractional
	  implicit object JXNUMIsJFRACTIONAL  extends JXNUM  //TODO should expect errors here
}