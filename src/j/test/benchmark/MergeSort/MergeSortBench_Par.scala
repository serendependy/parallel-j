package j.test.benchmark.MergeSort

import scala.testing.Benchmark

import j.lang.datatypes.JFuncRank
import j.lang.datatypes.JTypeMacros._

import j.lang.datatypes.function.JVerb
import j.lang.datatypes.function.JVerb1Type

import j.lang.datatypes.array.JArray
import j.lang.datatypes.array.ArrayImplicits._
import j.lang.datatypes.array.types.JNumberTypes._

import j.lang.primitives.JVerbs._

import j.lang.primitives.cheating.JCheating._

object MergeSortBench_Par extends MergeSortBench {
	override def setUp() {
	  JVerb.parallelFlag = true
	}
}
