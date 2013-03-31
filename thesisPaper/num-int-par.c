#include <stdio.h>
#include <omp.h>
#define NUM_STEPS 100000000

int main(int argc, char *argv[]) {

    double x, pi;
    double sum = 0.0;
    double step = 1.0/(double) NUM_STEPS;

	#pragma omp parallel for private(x) reduction(+:sum) schedule(static)
	for (int i=NUM_STEPS-1; i >= 0; --i) {
		x = i*step;
		sum = sum + 4.0/(1.0+x*x);
	}

	pi = step * sum;

	printf("pi %lf\n", pi);
    return 0;
}
