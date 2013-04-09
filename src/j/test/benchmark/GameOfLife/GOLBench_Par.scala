package j.test.benchmark.GameOfLife

import j.lang.datatypes.function.JVerb

import collection.parallel.ForkJoinTasks.defaultForkJoinPool

object GOLBench_Par extends GOLBench {
	override def setUp() {
	  JVerb.parallelFlag = true
	  defaultForkJoinPool.setParallelism(4)
	}
}
