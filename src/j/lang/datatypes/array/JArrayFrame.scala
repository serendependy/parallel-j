package j.lang.datatypes.array

import scala.collection.mutable.Stack

import j.lang.datatypes.array.types.JNumberTypes._
import j.lang.datatypes.array.JArrayFlag._
import j.lang.datatypes.JFuncRank

object JArrayFrame {
  def apply[T <% JArrayType : Manifest](frameLevels: List[JNumber], jar: JArray[T]) = {
    new JArrayFrame(frameLevels.map(jnum => jnum match {
      case i: JInt => i.v
      case inf: JInfinite => jar.rank
      case _ => throw new Exception() //TODO domain error
    }), jar)
  }
  
  def apply[T <% JArrayType : Manifest](rank: JNumber, jar: JArray[T]): JArrayFrame[T] = JArrayFrame(List(rank), jar)
  
  def createFrames[T <% JArrayType : Manifest](rs: List[JFuncRank], jar: JArray[T]) = JArrayFrame(rs.map(_ r1), jar)
  def createFrames[T <% JArrayType : Manifest, U <% JArrayType : Manifest](rs: List[JFuncRank], jar1: JArray[T], jar2: JArray[U]): (JArrayFrame[T], JArrayFrame[U]) = {
    (JArrayFrame(rs.map(_ r2), jar1), JArrayFrame(rs.map(_ r3), jar2))
  }
}

class JArrayFrame[T <% JArrayType : Manifest] private(val frameLevels: List[Int], val jar: JArray[T]) {
  val frames = {
    var tempShape = jar.shape
    (for (rank <- frameLevels.reverse) yield {
    	val frame = tempShape.take(tempShape.length - rank)
    	tempShape = tempShape.drop(tempShape.length - rank)
    	frame
    }).toList :+ tempShape
  }
  val cellShape = frames.last
  val cellSize = cellShape.foldLeft(1)(_ * _)
  val frameSize = jar.shape.foldLeft(1)(_ * _) / cellSize
 
  	def mapOnCells[U <% JArrayType : Manifest](func: JArray[T] => JArray[U]): JArray[U] = {
      
  	  val newCells = (for (fr <- 0 to frameSize) yield {
  	    func(JArray(jar.jaType, cellShape, jar.ravel.view(fr, fr + cellSize).toArray))
  	  })
  	  val newShape = frames.dropRight(1).foldLeft(List[Int]())(_ ++ _) ++ newCells(0).shape
  	  JArray(newCells(0).jaType, newShape, newCells.foldLeft(Array[U]())(_ ++ _.ravel) )
  	  
  	}
  
  	def shapeAgreement(other: JArrayFrame[_]):Option[List[Int]] = {
  	  if (this.frames.length != other.frames.length) None
  	  else {
  	    (this.frames, other.frames).zipped.map((l1, l2) => {
  	      if (l1.length > l2.length) {
  	        if (l1.take(l2.length) == l2) Some(l1)
  	        else None
  	      }
  	      else {
  	        if (l2.take(l1.length) == l1) Some(l2)
  	        else None
  	      }
  	    }).foldLeft(Option(List[Int]()))( (o,l) => {
  	      o match {
  	        case None => None
  	        case Some(lp) => l match {
  	          case None => None
  	          case Some(ln) => Some(lp ++ ln)
  	        }
  	      }
  	    })
  	  }
  	}
  
	override def toString() = {
	  "" + frames + "\n" + jar
	}  
}