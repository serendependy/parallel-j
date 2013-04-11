for n in {100..316}
	do
		for t in 1 2 4 8
			do	
				scala -classpath ../obj j.test.benchmark.GameOfLife.GOLBench_Par $n 5 $t
			done
	done
