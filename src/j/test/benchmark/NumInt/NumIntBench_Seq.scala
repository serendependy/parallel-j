package j.test.benchmark.NumInt

import j.lang.datatypes.function.JVerb

object NumIntBench_Seq extends NumIntBench {

	override def setUp() {
	  JVerb.parallelFlag = false
	}
}