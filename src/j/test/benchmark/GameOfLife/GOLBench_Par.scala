package j.test.benchmark.GameOfLife

import j.lang.datatypes.function.JVerb

object GOLBench_Par extends GOLBench {
	override def setUp() {
	  JVerb.parallelFlag = true
	}
}
