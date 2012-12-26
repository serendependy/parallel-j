package j.lang.datatypes.array

import j.util.CMacroType
import j.util.Rational

import j.lang.datatypes.array.types.numeric.JNumber

sealed class JArrayType[T]

object JArrayType {

	  //the three type families in J
	  implicit object JCHAR		extends JArrayType[Char]

	  class Box[T: JArrayType](val value: T)
	  implicit object JBOX	extends JArrayType[Box[_]]
	  
	  implicit object JNUMERIC extends JArrayType[JNumber]
}