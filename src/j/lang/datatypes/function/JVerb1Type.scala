package j.lang.datatypes.function

import j.lang.datatypes.JFuncRank
import j.lang.datatypes.JTypeMacros._

import j.lang.datatypes.array.JArray
import j.lang.datatypes.array.JArrayType

import j.lang.datatypes.array.types.JNumberTypes._

class JVerb1Type[T <% JArrayType : Manifest](rep: String, ranks: List[JFuncRank],
  mimpl: JArray[T] => JArray[T], dimpl: (JArray[T], JArray[T]) => JArray[T],
  domain: JType) extends 
  JVerb[T,T,T,T,T](rep, ranks, mimpl, dimpl, domain, domain, domain)