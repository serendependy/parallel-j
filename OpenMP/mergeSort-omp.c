#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <omp.h>

#define SWAP(a,b) {int temp = a; a = b; b = temp;}

#define SIZE 1048576

void setUp(int a[], int size);
void tearDown(double start, double end, int a[], int size);
void merge(int a[], int size, int temp[]);
void mergesort_serial(int a[], int size, int temp[]);
void mergesort_parallel_omp
(int a[], int size, int temp[], int threads);

int main() {
	int a[SIZE];
	int temp[SIZE];
	double startTime, endTime;
	int num_threads;

	#pragma omp parallel
	{
		#pragma omp master
		{
			num_threads = omp_get_num_threads();
		}
	}

	setUp(a, SIZE);

	

	startTime = omp_get_wtime();
	mergesort_parallel_omp(a, SIZE, temp, num_threads);
	endTime = omp_get_wtime();

	tearDown(startTime, endTime, a, SIZE);
}

void setUp(int a[], int size){
	int i;

	srand(time(NULL));
	for (i = 0; i<size; ++i) {
		a[i] = rand() % size;
	}
	return;
}

void tearDown(double start, double end, int a[], int size) {
	int sorted = 1;
	int i;

	printf("Time to execute: %f\n", end-start);
	
	for (i = 0; i < size-1; ++i) {
		sorted &= (a[i] <= a[i+1]);
	}

	printf("Array sorted: %d\n", sorted);

	#pragma omp parallel
	{
		#pragma omp master
		{
			printf("Num threads: %d\n", omp_get_num_threads());
		}
	}
}

void merge(int a[], int size, int temp[]) {
	int i1 = 0;
	int i2 = size / 2;
	int it = 0;

	while(i1 < size/2 && i2 < size) {
		if (a[i1] <= a[i2]) {
			temp[it] = a[i1];
			i1 += 1;
		}
		else {
			temp[it] = a[i2];
			i2 += 1;
		}
		it += 1;
	}

	while (i1 < size/2) {
	    temp[it] = a[i1];
	    i1++;
	    it++;
	}
	while (i2 < size) {
	    temp[it] = a[i2];
	    i2++;
	    it++;
	}

	memcpy(a, temp, size*sizeof(int));

}

void mergesort_serial(int a[], int size, int temp[]) {
	int i;

	if (size == 2) { 
		if (a[0] <= a[1])
			return;
		else {
			SWAP(a[0], a[1]);
			return;
		}
	}

	mergesort_serial(a, size/2, temp);
	mergesort_serial(a + size/2, size - size/2, temp);
	merge(a, size, temp);
}

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
