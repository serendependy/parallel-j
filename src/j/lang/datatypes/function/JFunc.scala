package j.lang.datatypes.function

import j.lang.datatypes._
import j.lang.datatypes.JTypeMacros._
import j.lang.datatypes.array._

import j.util.CMacroType._

abstract class JFunc[M: JArrayType, D1: JArrayType, D2: JArrayType,
    MR: JArrayType, DR: JArrayType]
	(val rep: String, val funcType: JType, val ranks: List[JFuncRank],
    val mdomain: JType, val d1domain: JType, val d2domain: JType) {

  import JFunc._
  
  val mInDomain = inDomain(mdomain)_
  val d1InDomain= inDomain(d1domain)_
  val d2InDomain= inDomain(d2domain)_
  
  protected def monadImpl(y: JArray[M]): JArray[MR]
  protected def dyadImpl(x: JArray[D1], y: JArray[D2]): JArray[DR]
  
  def monad(y: JArray[M]):JArray[MR] = {
    if (mInDomain(y.jaType) ) {
      monadImpl(y)
    }
    else {
      throw new Exception() //TODO make this a domain error
    }
  }
  
  def dyad(x: JArray[D1], y: JArray[D2]): JArray[DR] = {
    if (d1InDomain(x.jaType) && d2InDomain(x.jaType)) {
      dyadImpl(x,y)
    }
    else {
      throw new Exception() //TODO make this domain error
    }
  }
}

object JFunc {
  def inDomain(domain: JType)(argType: JType) = argType isA domain 
}