for n in 14 15 16
	do
		scala -classpath ../obj j.test.benchmark.MergeSort.MergeSortBench_Seq $n 5
	done

for n in {10000..100000..10000}
	do
		scala -classpath ../obj j.test.benchmark.NumInt.NumIntBench_Seq $n 5
	done
