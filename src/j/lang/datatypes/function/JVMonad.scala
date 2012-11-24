package j.lang.datatypes.function

import j.lang.datatypes.JTypeMacros._
import j.lang.datatypes.array._
import j.lang.datatypes.array.JArrayType._

class JVMonad[T: JArrayType, V: JArrayType]
		(rep: String, leftdom: JType, val rightdom: JType, val impl: T => V) 
		extends	JVerb[T,V](rep, leftdom) {

}