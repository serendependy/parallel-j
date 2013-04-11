for n in 14 15 16
	do
		for t in {2 4 8}
			do
				scala -classpath ../obj j.test.benchmark.MergeSort.MergeSortBench_Par $n 5 $t
			done
	done

for n in {10000..100000..10000}
	do
		for t in {2 4 8}
			do
				scala -classpath ../obj j.test.benchmark.NumInt.NumIntBench_Par $n 5 $t
			done
	done
