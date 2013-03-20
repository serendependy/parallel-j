package j.lang.datatypes.function

import j.lang.datatypes._
import j.lang.datatypes.JDataType
import j.lang.datatypes.JTypeMacros._
import j.lang.datatypes.array._

import j.util.CMacroType._

abstract class JFunc[M <: JDataType, D1 <: JDataType, D2 <: JDataType,
    MR <: JDataType, DR <: JDataType](
    val rep: String, val funcType: JTypeMacro, 
	val mdomain: JTypeMacro, val d1domain: JTypeMacro, val d2domain: JTypeMacro)
	extends JDataType(funcType) {

  import JFunc._
  
  val mInDomain = inDomain(mdomain)_
  val d1InDomain= inDomain(d1domain)_
  val d2InDomain= inDomain(d2domain)_
  
  def monad[T <: M](y: T): MR
  def dyad[T1 <: D1, T2 <: D2](x: T1, y: T2): DR

}

object JFunc {
  def inDomain(domain: JTypeMacro)(argType: JTypeMacro) = argType isA domain 
}