#MergeSort
for n in 14 15 16
	do
	    echo "Merge Sort with size 2 ^ $n"
		scala -classpath ../obj j.test.benchmark.MergeSort.MergeSortBench_Seq $n 6
	done

for n in 14 15 16
	do
		for t in 2 4 8 16
			do
			    echo "Merge Sort with size 2 ^ $n and threads $t"
				scala -classpath ../obj j.test.benchmark.MergeSort.MergeSortBench_Par $n 5 $t
			done
	done

#Numerical integration
for n in 10000 20000 30000 40000 50000 60000 70000 80000 90000 100000
	do
	    echo "NumInt with size $n"
		scala -classpath ../obj j.test.benchmark.NumInt.NumIntBench_Seq $n 6
	done



for n in 10000 20000 30000 40000 50000 60000 70000 80000 90000 100000
	do
		for t in 2 4 8 16
			do
			    echo "Num Int with size $n and threads $t"
				scala -classpath ../obj j.test.benchmark.NumInt.NumIntBench_Par $n 5 $t
			done
	done
	
#Game of life
for n in {100..316}
	do
	    echo "Game of Life with board size $n"
		scala -classpath ../obj j.test.benchmark.GameOfLife.GOLBench_Seq $n 5
	done

for n in {100..316}
	do
		for t in 2 4 8 16
			do	
			    echo "Game of life par with board size $n and threads $t"
				scala -classpath ../obj j.test.benchmark.GameOfLife.GOLBench_Par $n 6 $t
			done
	done
