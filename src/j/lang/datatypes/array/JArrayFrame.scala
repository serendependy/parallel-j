package j.lang.datatypes.array

import scala.collection.mutable.Stack

import j.lang.datatypes.array.types.JNumberTypes._
import j.lang.datatypes.array.JArrayFlag._
import j.lang.datatypes.JFuncRank

object JArrayFrame {
  def apply(frameLevels: List[JNumber], jar: JArray[_]) = {
    new JArrayFrame(frameLevels.map(jnum => jnum match {
      case i: JInt => i.v
      case inf: JInfinite => jar.rank
      case _ => throw new Exception() //TODO domain error
    }), jar)
  }
  
  def apply(rank: JNumber, jar: JArray[_]): JArrayFrame = JArrayFrame(List(rank), jar)
  
  def createFrames(rs: List[JFuncRank], jar: JArray[_]) = JArrayFrame(rs.map(_ r1), jar)
  def createFrames(rs: List[JFuncRank], jar1: JArray[_], jar2: JArray[_]): (JArrayFrame, JArrayFrame) = {
    (JArrayFrame(rs.map(_ r2), jar1), JArrayFrame(rs.map(_ r3), jar2))
  }
}

class JArrayFrame private(val frameLevels: List[Int], val jar: JArray[_]) {
  lazy val frames = {
    var tempShape = jar.shape
    (for (rank <- frameLevels.reverse) yield {
    	val frame = tempShape.take(tempShape.length - rank)
    	tempShape = tempShape.drop(tempShape.length - rank)
    	frame
    }).toList :+ tempShape
  }
  
  	def shapeAgreement(other: JArrayFrame):Option[List[Int]] = {
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