package j.lang.datatypes.function

import j.lang.datatypes.JFuncRank
import j.lang.datatypes.JTypeMacros._
import j.lang.datatypes.array.JArray
import j.lang.datatypes.array.JArrayType
import j.lang.datatypes.array.JArrayFrame

import j.lang.datatypes.array.types.JNumberTypes._

abstract class JVerb[M <: JArrayType : Manifest, D1 <: JArrayType : Manifest, D2 <: JArrayType : Manifest,
  MR <: JArrayType : Manifest, DR <: JArrayType : Manifest]
  (rep: String, val ranks: List[JFuncRank], mdomain: JTypeMacro, d1domain: JTypeMacro, d2domain: JTypeMacro) extends 
  JFunc[JArray[M], JArray[D1], JArray[D2], JArray[MR], JArray[DR]](rep, jVERB, mdomain, d1domain, d2domain) {

  override def monad[T <: JArray[M]](y: T) = { //some testing with types and shape
	  val jaf = JArrayFrame(ranks.map(_ r1), y)
	  val newCells = (for (fr <- 0 until jaf.frameSize) yield {
	    monadImpl(JArray(jaf.jar.jaType, jaf.cellShape, jaf.jar.ravel.slice(fr*jaf.cellSize, (1+fr)*jaf.cellSize)))
	  })
	  val newShape = jaf.frames.dropRight(1).foldLeft(List[Int]())(_ ++ _) ++ newCells(0).shape
	  JArray(newCells(0).jaType, newShape, newCells.foldLeft(Vector[MR]())(_ ++ _.ravel))
	}
	
  def addRanks(r: JFuncRank) = {
    val thisotherthing = this
    new JVerb[M,D1,D2,MR,DR](
        rep + "(\"" + r + ")",
        ranks :+ r,
        mdomain, d1domain, d2domain) {
     
      override def monadImpl[T <: M : Manifest](y: JArray[T]) = thisotherthing.monadImpl(y)
      override def dyadImpl[T1 <: D1 : Manifest, T2 <: D2 : Manifest](x: JArray[T1], y: JArray[T2]) = thisotherthing.dyadImpl(x, y)
    }
  }
  
	override def dyad[T1 <: JArray[D1], T2 <: JArray[D2]](x: T1, y: T2) = {
	  val jafx = JArrayFrame(ranks.map(_ r2), x)
	  val jafy = JArrayFrame(ranks.map(_ r3), y)
	  
	  jafx.shapeAgreement(jafy) match {
	    case None => throw new Exception() //TODO shape error
	    case Some(agree) => {
	      val xreframed = jafx.shapeToNewFrame(agree)
	      val yreframed = jafy.shapeToNewFrame(agree)
	      
	      val cellShape = agree.last
	      val cellSize  = cellShape.foldLeft(1)(_ * _)
	      val frameSize = xreframed.shape.foldLeft(1)(_ * _) / cellSize
	      
	      val newCells = (for (fr <- 0 until frameSize) yield {
	        dyadImpl(JArray(jafx.jar.jaType, cellShape, xreframed.ravel.slice(fr*cellSize, (1+fr)*cellSize)),
	        		 JArray(jafy.jar.jaType, cellShape, yreframed.ravel.slice(fr*cellSize, (1+fr)*cellSize)) )
	      })
	      val newShape = agree.dropRight(1).foldLeft(List[Int]())(_ ++ _) ++ newCells(0).shape
	      JArray(newCells(0).jaType, newShape, newCells.foldLeft(Vector[DR]())(_ ++ _.ravel))
	    }
	  }
	}
	
	protected def monadImpl[T <: M : Manifest](y: JArray[T]): JArray[MR]
	protected def dyadImpl[T1 <: D1 : Manifest, T2 <: D2 : Manifest](x: JArray[T1], y: JArray[T2]): JArray[DR]
}