package j.test.benchmark.NumInt

import collection.parallel.ForkJoinTasks.defaultForkJoinPool

import j.lang.datatypes.function.JVerb

object NumIntBench_Par extends NumIntBench {
  override def setUp() {
    JVerb.parallelFlag = true
    defaultForkJoinPool.setParallelism(4)
  }
}