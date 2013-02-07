package j.lang.datatypes.function

import j.lang.datatypes.JFuncRank
import j.lang.datatypes.JTypeMacros._

import j.lang.datatypes.array.JArray
import j.lang.datatypes.array.JArrayType

import j.lang.datatypes.array.types.JNumberTypes._

class JVerb2Type[T <% JArrayType : Manifest, S <% JArrayType](rep: String, ranks: List[JFuncRank],
    mimple: JArray[T] => JArray[T], dimple: (JArray[S], JArray[T]) => JArray[T],
    d1: JType, d2: JType) extends 
    JVerb[T,S,T,T,T](rep, ranks, mimple, dimple, d1, d2, d1){

}