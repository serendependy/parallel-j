package j.lang.datatypes.function

import j.lang.datatypes.JFuncRank
import j.lang.datatypes.JTypeMacros._

import j.lang.datatypes.array.JArray
import j.lang.datatypes.array.JArrayType

import j.lang.datatypes.array.types.JNumberTypes._

abstract class JVerb2Type[T <: JArrayType : Manifest, S <: JArrayType : Manifest](rep: String, ranks: List[JFuncRank],
    d1: JTypeMacro, d2: JTypeMacro) extends 
    JVerb[T,S,T,T,T](rep, ranks, d1, d2, d1){

}