package j.test.benchmark.GameOfLife

import scala.testing.Benchmark

import j.lang.datatypes.function.JVerb

object GOLBench_Seq extends GOLBench {
  
  override def setUp() {
    JVerb.parallelFlag = false
  }

}
