package j.test.benchmark.NumInt

import scala.testing.Benchmark

import j.lang.datatypes.function.JVerb

import j.lang.datatypes.array.JArray
import j.lang.datatypes.array.ArrayImplicits._
import j.lang.datatypes.array.types.JNumberTypes._

import j.lang.primitives.JVerbs._

object NumIntBench_Par extends NumIntBench {
  override def setUp() {
    JVerb.parallelFlag = true
  }
}