package j.lang.datatypes.array

import j.lang.datatypes.array.types.JNumberTypes._
import j.lang.datatypes.array.JArrayFlag._


object JArrayFrame {
	def apply[T <% JArrayType : Manifest](itemRank: JNumber, jar: JArray[T]): JArrayFrame = itemRank match {
	  case inf: JInfinite => new JArrayFrame(List(), Array(jar))
	  case intRank: JInt => {
	    if (intRank.v >= jar.rank)
	      new JArrayFrame(List(), Array(jar))
	    else {
	      val cellShape = jar.shape.drop(jar.rank - intRank.v)
	      val numInCells = cellShape.fold(1)(_ * _)
	      val frameShape = jar.shape.take(jar.rank - intRank.v)
	      val numInFrame = frameShape.fold(1)(_ * _)
	      new JArrayFrame(frameShape, Array.tabulate(numInFrame)(
	        (i: Int) => {
	           JArray(afNONE, jar.jaType, numInCells, 0, cellShape,
	             Array.tabulate(numInCells)((j: Int) => jar.ravel((i * numInCells) + j))
	           )
	         }
	      )
	    )
	    }
	  }
	  case _ => throw new Exception() //TODO domain error
	}
}

class JArrayFrame(val frameShape: List[Int], val cells: Array[JArray[_]]) {
	val numFrames = cells.length
	val cellShape = cells(0).shape //TODO make sure there's always at least one
}