package j.lang.datatypes.function

import j.lang.datatypes.JFuncRank
import j.lang.datatypes.JTypeMacros._
import j.lang.datatypes.array.JArray
import j.lang.datatypes.array.JArrayType
import j.lang.datatypes.array.JArrayFrame

import j.lang.datatypes.array.types.JNumberTypes._

abstract class JVerb[M <% JArrayType : Manifest, D1 <% JArrayType : Manifest, D2 <% JArrayType : Manifest,
  MR <% JArrayType : Manifest, DR <% JArrayType : Manifest]
  (rep: String, val ranks: List[JFuncRank], mdomain: JTypeMacro, d1domain: JTypeMacro, d2domain: JTypeMacro) extends 
  JFunc[JArray[M], JArray[D1], JArray[D2], JArray[MR], JArray[DR]](rep, jVERB, mdomain, d1domain, d2domain) {
  
}