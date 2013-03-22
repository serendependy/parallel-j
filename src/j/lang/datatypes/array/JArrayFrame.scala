package j.lang.datatypes.array

import scala.collection.mutable.Stack

import j.lang.datatypes.array.types.JNumberTypes._
import j.lang.datatypes.array.JArrayFlag._
import j.lang.datatypes.JFuncRank

object JArrayFrame {
  def apply[T <: JArrayType : Manifest](frameLevels: List[JNumber], jar: JArray[T]) = {
    val intFrameLevels = frameLevels.map(_ match {
      case i: JInt => i.v
      case inf: JInfinite => jar.rank
      case _ => throw new Exception() //TODO domain error
    })
	  val frames = {
		  var tempShape = jar.shape
    	  (for (r <- intFrameLevels.reverse) yield {
    	    val rank = if (r >= 0) r else tempShape.length - r
    		val frame = tempShape.take(tempShape.length - rank)
    		tempShape = tempShape.drop(tempShape.length - rank)
    		frame
    	}).toList :+ tempShape
  	}
    new JArrayFrame(frames, jar)
  }
  
  def apply[T <: JArrayType : Manifest](rank: JNumber, jar: JArray[T]): JArrayFrame[T] = JArrayFrame(List(rank), jar)
  
  def createFrames[T <: JArrayType : Manifest](rs: List[JFuncRank], jar: JArray[T]) = JArrayFrame(rs.map(_ r1), jar)
  def createFrames[T <: JArrayType : Manifest, U <: JArrayType : Manifest](rs: List[JFuncRank], jar1: JArray[T], jar2: JArray[U]): (JArrayFrame[T], JArrayFrame[U]) = {
    (JArrayFrame(rs.map(_ r2), jar1), JArrayFrame(rs.map(_ r3), jar2))
  }
}

class JArrayFrame[T <: JArrayType : Manifest] private(val frames: List[List[Int]], val jar: JArray[T]) {
  lazy val cellShape = frames.last
  lazy val cellSize = cellShape.foldLeft(1)(_ * _)
  lazy val frameSize = jar.shape.foldLeft(1)(_ * _) / cellSize

  def shapeAgreement(other: JArrayFrame[_]):Option[List[List[Int]]] = {
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
  	      }//              Because the very last agreement is decided by the function
  	    }).dropRight(1).:+(Some(List[Int]())).foldLeft(Option(List[List[Int]]()))( (o,l) => {
  	      o match {
  	        case None => None
  	        case Some(lp) => l match {
  	          case None => None
  	          case Some(ln) => Some(lp :+ ln)
  	        }
  	      }
  	    })
  	  }
  	}
  
  	def shapeToNewFrame(nf: List[List[Int]]) = {
  	  val newFrame = nf.dropRight(1) :+ this.frames.last //TODO ugly hack for leaving function to do last agreement
  	  
  	  val niofs = this.frames.map(_.foldLeft(1)(_ * _)) // num in old frame
  	  val ninfs = newFrame.map(_.foldLeft(1)(_ * _))    // num in new frame
  	  val hmrs  = niofs.scanRight(1)(_ * _).drop(1)     // how many of this dimension
  	  val hmls  = ninfs.scanLeft(1)(_ * _).drop(1) 		// how many in this dimension
  	  
  	  def helper(ofs: List[List[Int]], nfs: List[List[Int]], hmrs: List[Int], hmls: List[Int], ninfs: List[Int], niofs: List[Int], acc: Vector[T]): Vector[T] = {
  	    println("---shapeToNewFrame: helper:\n" + ofs)
  	    println(nfs)
  	    println(hmrs)
  	    println(hmls)
  	    println(acc)
  	    if (ofs.isEmpty) acc
  	    else {
  	      val (of, nf, hmr, hml, ninf, niof) = (ofs.head, nfs.head, hmrs.head, hmls.head, ninfs.head, niofs.head)
  	      if (of.length == nf.length)
  	        helper(ofs.drop(1), nfs.drop(1), hmrs.drop(1), hmls.drop(1), ninfs.drop(1), niofs.drop(1), acc)
  	      else {
  	        val numCopies = ninf / niof
  	        val newacc = (0 until ((hml/ninf)*niof)).foldLeft(Vector[T]())((vec, i) => {
  	        	vec ++ (0 until numCopies).map( (k: Int) => { //TODO optimize
  	        	    (0 until hmr).map(
  	        	        (j: Int) => acc(j + i*hmr))
  	        	 }).foldLeft(Vector[T]())(_ ++ _)
  	        })
  	        helper(ofs.drop(1), nfs.drop(1), hmrs.drop(1), hmls.drop(1), ninfs.drop(1), niofs.drop(1), newacc)
  	      }
  	    }
  	  }
  	  
  	  JArray(jar.jaType, newFrame.foldLeft(List[Int]())(_ ++ _), helper(this.frames, newFrame, hmrs, hmls, ninfs, niofs, jar.ravel))
  	}
  	
	override def toString() = {
	  "" + frames + "\n" + jar
	}  
}
