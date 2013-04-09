package j.test.benchmark.MergeSort

import collection.parallel.ForkJoinTasks.defaultForkJoinPool

import j.lang.datatypes.function.JVerb

object MergeSortBench_Par extends MergeSortBench {
	override def setUp() {
	  JVerb.parallelFlag = true
	  defaultForkJoinPool.setParallelism(4)
	}
}
