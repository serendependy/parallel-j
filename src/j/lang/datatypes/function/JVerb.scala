package j.lang.datatypes.function

import j.lang.datatypes.JFuncRank
import j.lang.datatypes.JTypeMacros._
import j.lang.datatypes.array.JArray
import j.lang.datatypes.array.JArrayType
import j.lang.datatypes.array.JArrayFrame
import j.lang.datatypes.array.types.JNumberTypes._
import sun.reflect.generics.reflectiveObjects.NotImplementedException

abstract class JVerb[M <: JArrayType : Manifest, D1 <: JArrayType : Manifest, D2 <: JArrayType : Manifest,
  MR <: JArrayType : Manifest, DR <: JArrayType : Manifest]
  (rep: String, val ranks: List[JFuncRank], mdomain: JTypeMacro, d1domain: JTypeMacro, d2domain: JTypeMacro) extends 
  JFunc[JArray[M], JArray[D1], JArray[D2], JArray[MR], JArray[DR]](rep, jVERB, mdomain, d1domain, d2domain) {

  def apply[T <: JArray[M]](y: T) = monad(y)
  
  override def monad[T <: JArray[M]](y: T) = { //some testing with types and shape
	  val jaf = JArrayFrame(ranks.map(_ r1), y)
	  println("---monad call for " + rep + ": frame is:\n" + jaf)
	  println("   ranks: " + ranks)
	  println("   frameSize: " + jaf.frameSize)
	  val newCells = (for (fr <- 0 until jaf.frameSize) yield {
	    println("   " + fr + "-slice is " + jaf.jar.ravel.slice(fr*jaf.cellSize, (1+fr)*jaf.cellSize))
	    monadImpl(JArray(jaf.jar.jaType, jaf.cellShape, jaf.jar.ravel.slice(fr*jaf.cellSize, (1+fr)*jaf.cellSize)))
	  })
	  val newShape = jaf.frames.dropRight(ranks.length).foldLeft(List[Int]())(_ ++ _) ++ newCells(0).shape
//TODO ugly hack is ugly. Make sure to do one at a time, instead of all at once
//	  val newShape = jaf.frames.dropRight(1).foldLeft(List[Int]())(_ ++ _) ++ newCells(0).shape
	  JArray(newCells(0).jaType, newShape, newCells.foldLeft(Vector[MR]())(_ ++ _.ravel))
	}
	
  def addRanks(r: JFuncRank) = {
    val thisotherthing = this
    new JVerb[M,D1,D2,MR,DR](
        rep + "(\"" + r + ")",
        ranks :+ r,
        mdomain, d1domain, d2domain) {
     
      override def monadImpl[T <: M : Manifest](y: JArray[T]) = {
//        println("---addRanks private class\nCalling class's monadImpl")
        thisotherthing.monadImpl(y)
      }
      override def dyadImpl[T1 <: D1 : Manifest, T2 <: D2 : Manifest](x: JArray[T1], y: JArray[T2]) = thisotherthing.dyadImpl(x, y)
    }
  }
  //TODO leverage (implicit ev1: D1 =:= D2, ev2: D2 =:= DR)
  def insert(implicit ev1: JArray[D1] =:= JArray[D2], ev2: JArray[DR] =:= JArray[D2], ev3: JArray[D2] =:= JArray[D1]) = {
    import j.lang.datatypes.array.types.JNumberTypes._
    
    val thisotherthing = this
    new JVerb[D1,D1,D1,D1,D1](
        rep + "/",
        ranks :+ JFuncRank(JInfinity),
        mdomain, d1domain,d2domain
    ){

      protected override def monadImpl[T <: D1 : Manifest](y: JArray[T]) = y.numItemz match {
        case 0 => throw new Exception() //TODO should fetch identity element
        case 1 => y
        case n: Int => {
        	 (0 until n).map(j => ev1(y(j))).reduce((y1, y2) => {
        	  ev2(thisotherthing.dyad(ev3(y1), y2))
        	})
        }
      }
      
      protected override def dyadImpl[T1 <: D1 : Manifest, T2 <: D1 : Manifest](x: JArray[T1], y: JArray[T2]) = {
        throw new NotImplementedException()//TODO implement
      }
    }
  }
  
  		def agenda[MRR <: JArrayType : Manifest, DRR <: JArrayType : Manifest](
  		    f: JVerb[M,D1,D2,MRR,DRR], t: JVerb[M,D1,D2,MRR,DRR]) = {
  		  
  		}
//TODO for the lolz later
//  	def reflex(implicit ev1: JArray[D2] =:= JArray[D1], ev2: JArray[D1] =:= JArray[D2]) = {
//  	  val thisOtherThing = this
//  	  
//  	  new JVerb[D2, D2, D1, DR, DR](
//  	      rep + )
//  	}
  
  	def apply[T1 <: JArray[D1], T2 <: JArray[D2]](x: T1, y: T2) = dyad(x,y)
	override def dyad[T1 <: JArray[D1], T2 <: JArray[D2]](x: T1, y: T2) = {
	  val jafx = JArrayFrame(ranks.map(_ r2), x)
	  val jafy = JArrayFrame(ranks.map(_ r3), y)
	  
	  println("---dyad call for " + rep)
	  println("   xframe: " + jafx)
	  println("   yframe: " + jafy)
	  println("   ranks: " + ranks)
	  
	  jafx.shapeAgreement(jafy) match {
	    case None => throw new Exception() //TODO shape error
	    case Some(agree) => {
	      val xreframed = jafx.shapeToNewFrame(agree)
	      val yreframed = jafy.shapeToNewFrame(agree)
	      
/*	      val cellShape = agree.last
	      val cellSize  = cellShape.foldLeft(1)(_ * _)
	      val frameSize = xreframed.shape.foldLeft(1)(_ * _) / cellSize*/
	      
	      val xcellShape = jafx.frames.last
	      val xcellSize = xcellShape.foldLeft(1)(_ * _)
	      val ycellShape = jafy.frames.last
	      val ycellSize  = ycellShape.foldLeft(1)(_ * _)
	      val frameSize  = agree.init.foldLeft(1)(_ * _.foldLeft(1)(_ * _))
	      
	      val newCells = (for (fr <- 0 until frameSize) yield {
	        dyadImpl(JArray(jafx.jar.jaType, xcellShape, xreframed.ravel.slice(fr*xcellSize, (1+fr)*xcellSize)),
	        		 JArray(jafy.jar.jaType, ycellShape, yreframed.ravel.slice(fr*ycellSize, (1+fr)*ycellSize)) )
	      })
	      val newShape = agree.dropRight(1).foldLeft(List[Int]())(_ ++ _) ++ newCells(0).shape
	      JArray(newCells(0).jaType, newShape, newCells.foldLeft(Vector[DR]())(_ ++ _.ravel))
	    }
	  }
	}
	
	protected def monadImpl[T <: M : Manifest](y: JArray[T]): JArray[MR]
	protected def dyadImpl[T1 <: D1 : Manifest, T2 <: D2 : Manifest](x: JArray[T1], y: JArray[T2]): JArray[DR]
}