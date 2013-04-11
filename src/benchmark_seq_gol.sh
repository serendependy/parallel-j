for n in {100..316}
	do
		scala -classpath ../obj j.test.benchmark.GameOfLife.GOLBench_Seq $n 5
	done
