#include <math.h>
#include <R.h>

void foo(int *nin, double *x)
{
	int n = nin[0];

	int i;
	warning("BLA");
	printf("Trying PrintF here");
	for (i=0; i<n; i++)
		x[i] = x[i] * x[i];

}