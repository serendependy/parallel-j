void mergesort_parallel_omp
(int a[], int size, int temp[], int threads) {
	if ( threads == 1) {
		mergesort_serial(a, size, temp);
	}
	else if (threads > 1) {
		#pragma omp parallel sections
		{
			#pragma omp section
			mergesort_parallel_omp(a, size/2, temp, threads/2);
			#pragma omp section
			mergesort_parallel_omp(a + size/2, size-size/2,
				temp + size/2, threads-threads/2);
		}

		merge(a, size, temp);
	} // threads > 1
}

