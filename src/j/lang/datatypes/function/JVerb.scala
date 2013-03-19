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
	  monadImpl(y)
	}
	
	override def dyad[T1 <: JArray[D1], T2 <: JArray[D2]](x: T1, y: T2) = {
	  dyadImpl(x,y)
	}
	
	private def mapOnCells[TM <: M](y: JArrayFrame[TM]) = {
	  
	}
	
	protected def monadImpl[T <: M : Manifest](y: JArray[T]): JArray[MR]
	protected def dyadImpl[T1 <: D1 : Manifest, T2 <: D2 : Manifest](x: JArray[T1], y: JArray[T2]): JArray[DR]
	
//def mapOnCells[R <% JArrayType : Manifest](func: JArray[T] => JArray[R]): JArray[R] = {
//
//  	  val newCells = (for (fr <- 0 until frameSize) yield {
//  	    func(JArray(jar.jaType, cellShape, jar.ravel.slice(fr*cellSize, (1+fr)*cellSize)))
//  	  })
//  	  val newShape = frames.dropRight(1).foldLeft(List[Int]())(_ ++ _) ++ newCells(0).shape
//  	  JArray(newCells(0).jaType, newShape, newCells.foldLeft(Vector[R]())(_ ++ _.ravel) )
//  	  
//  	}
//  
//  def mapOnCells[U <% JArrayType : Manifest, R <% JArrayType : Manifest](
//      func: (JArray[T], JArray[U]) => JArray[R], other: JArrayFrame[U]) = {
//  		val resShapeAgree = this.shapeAgreement(other)
//  		resShapeAgree match {
//  		  case None => throw new Exception() //TODO shape error
//  		  case Some(sv) => {
//		    
//  		    val thisReframed = this.shapeToNewFrame(sv)
//  		    val otherReframed= other.shapeToNewFrame(sv)
//
//  		    val cellShape = sv.last
//  		    val cellSize  = cellShape.foldLeft(1)(_ * _)
//  		    val frameSize = thisReframed.shape.foldLeft(1)(_ * _) / cellSize 
//  		    
//  		    val newCells = (for (fr <- 0 until frameSize) yield {
//  		      func(JArray(jar.jaType, cellShape, thisReframed.ravel.slice(fr*cellSize, (1+fr)*cellSize)),
//  		           JArray(other.jar.jaType, cellShape, otherReframed.ravel.slice(fr*cellSize, (1+fr)*cellSize) ))
//  		    })
//  		    val newShape = sv.dropRight(1).foldLeft(List[Int]())(_ ++ _) ++ newCells(0).shape
//  		    JArray(newCells(0).jaType, newShape, newCells.foldLeft(Vector[R]())(_ ++ _.ravel) )
//  		  }
//  		}
}