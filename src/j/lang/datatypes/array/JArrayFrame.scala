package j.lang.datatypes.array

import j.lang.datatypes.array.types.JNumberTypes._

object JArrayFrame {
	def apply(itemRank: JNumber, jar: JArray[_]): JArrayFrame = itemRank match {
	  case inf: JInfinite => new JArrayFrame(List(), Array(jar))
	  case intRank: JInt => {
	    if (intRank.v >= jar.rank)
	      new JArrayFrame(List(), Array(jar))
	    else {
	      val cellShape = jar.shape.drop(jar.rank - intRank.v)
	      val numCells = cellShape.fold(1)(_ * _)
	      val frameShape = jar.shape.take(jar.rank - intRank.v)
	      new JArrayFrame(frameShape, Array.tabulate(numCells)( (i: Int) => {
	        JArray() //TODO start here
	      }
	          ))
	    }
	  }
	  case _ => throw new Exception() //TODO domain error
	}
}

class JArrayFrame(val frameShape: List[Int], val cells: Array[JArray[_]]) {
	val numFrames = cells.length
	val cellShape = cells(0).shape //TODO make sure there's always at least one
}