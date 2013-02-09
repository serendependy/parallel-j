package j.lang.datatypes.function

import j.lang.datatypes.JFuncRank
import j.lang.datatypes.JTypeMacros._
import j.lang.datatypes.array.JArray
import j.lang.datatypes.array.JArrayType
import j.lang.datatypes.array.JArrayFrame

import j.lang.datatypes.array.types.JNumberTypes._

class JVerb[M <% JArrayType : Manifest, D1 <% JArrayType, D2 <% JArrayType,
  MR <% JArrayType : Manifest, DR <% JArrayType](rep: String, ranks: List[JFuncRank],
  mimpl: JArray[M] => JArray[MR], dimpl: (JArray[D1], JArray[D2]) => JArray[DR],
  mdomain: JType, d1domain: JType, d2domain: JType) extends 
  JFunc[M,D1,D2,MR,DR](rep, jVERB, ranks, mdomain, d1domain, d2domain)
{  
	override def monadImpl[ST <: M : Manifest](y: JArray[ST]) = {
	  val yframed = JArrayFrame.createFrames(ranks, y)
	  yframed.mapOnCells(mimpl)
	}
	override def dyadImpl(x: JArray[D1],y: JArray[D2])  = dimpl(x,y) 
}