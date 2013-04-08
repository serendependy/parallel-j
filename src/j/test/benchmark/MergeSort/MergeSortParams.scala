package j.test.benchmark.MergeSort

import j.lang.datatypes.array.types.JNumberTypes._

import j.lang.datatypes.array.ArrayImplicits._
import j.lang.datatypes.array.JArray
import j.lang.primitives.JVerbs._

object MergeSortParams {
	val y = {
		val numToDeal = JArray.scalar[JInt,Int](16384)
		rollDeal(numToDeal, numToDeal)
	}
}
