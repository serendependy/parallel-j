package j.lang.datatypes.function

import j.lang.datatypes.JFuncRank
import j.lang.datatypes.JTypeMacros._
import j.lang.datatypes.array.JArray
import j.lang.datatypes.array.JArrayType

class JHigherFunc[M: JArrayType, D1: JArrayType, D2: JArrayType,
  MR: JArrayType, DR: JArrayType](rep: String, ranks: List[JFuncRank],
      mimpl: JArray[M] => JArray[MR], dimpl: (JArray[D1], JArray[D2]) => JArray[DR],
  mdomain: JType, d1domain: JType, d2domain: JType) extends 
  JFunc[M,D1,D2,MR,DR](rep, jADV|jCONJ, ranks, mdomain, d1domain, d2domain) {

  	override def monadImpl(y: JArray[M]) = mimpl(y)
	override def dyadImpl(x: JArray[D1],y: JArray[D2])  = dimpl(x,y) 
}