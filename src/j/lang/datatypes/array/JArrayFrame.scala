package j.lang.datatypes.array

import scala.collection.mutable.Stack

import j.lang.datatypes.array.types.JNumberTypes._
import j.lang.datatypes.array.JArrayFlag._

object JArrayFrame {
  def apply(frameLevels: List[JNumber], jar: JArray[_]) = {
    new JArrayFrame(frameLevels.map(jnum => jnum match {
      case i: JInt => i.v
      case inf: JInfinite => jar.rank
      case _ => throw new Exception() //TODO domain error
    }), jar)
  }
  
  def apply(rank: JNumber, jar: JArray[_]): JArrayFrame = JArrayFrame(List(rank), jar)
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
//	      )
//	    )
//	    }
//	  }
//	  case _ => throw new Exception() //TODO domain error
//	}
//	
//	def apply[T <% JArrayType : Manifest](itemRanks: List[JNumber], jar: JArray[T]):JArrayFrame = {
//	  if (itemRanks isEmpty) JArrayFrame(jar) else
//	  itemRanks(0) match {
//	    case inf: JInfinite => new JArrayFrame(List(), Array(Right(jar)))
//	    case intRank: JInt => {
//	      if (intRank.v >= jar.rank)
//	        new JArrayFrame(List(), Array(Right(jar)) )
//	      else {
//	    	  val cellShape = jar.shape.drop(jar.rank - intRank.v)
//	    	  val numInCells = cellShape.fold(1)(_ * _)
//	    	  val frameShape = jar.shape.take(jar.rank - intRank.v)
//	    	  val numInFrame = frameShape.fold(1)(_ * _)
//	    	  new JArrayFrame(frameShape, Array.tabulate(numInFrame)(
//	    		 (i: Int) => {
//	    		   val mjar = JArray(afNONE, jar.jaType, numInCells, 0, cellShape,
//	                    Array.tabulate(numInCells)((j: Int) => jar.ravel((i * numInCells) + j)))
//	    		   if (itemRanks.length == 1)
//	    		     Right(mjar)
//	               else
//	                 Left(JArrayFrame(itemRanks.drop(1), mjar))
//	             }
//	          ))
//	      }
//	    }
//	    case _ => throw new Exception() //TODO make domain error
//	  }
//	}
//	
//	def apply[T <% JArrayType : Manifest](jar: JArray[T]) = new JArrayFrame(List(), Array(Right(jar)))
//	
//}
//
//class JArrayFrame(val frameShape: List[Int], val cells: Either[Array[JArrayFrame], Array[JArray[_]]]) {
//	val numFrames = cells.fold(_ length, _ length)
//	val cellShape = cells match {
//	  case Left(ajaf) => 
//	}
////	val cellShape = if (cells isEmpty) List() else cells(0) match {
////	  case Left(jaf) => jaf.numFrames
////	  case Right(jar)=> jar.shape
////	}
//	
//	override def toString() = {
//	  "" + frameShape + "\n"+ cellShape + "\n" + cells.map(_ match {
//	    case Left(jaf) => jaf.toString()
//	    case Right(jar)=> jar.toString()
//	  }).mkString("\n----\n")
//	}
//}