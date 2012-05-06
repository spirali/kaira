#include <stdio.h>
#include "factorial.h"

int main(int argc, char **argv) {

	calib_init(argc, argv);

	int x = 10;
	factorial(x);
	printf("%d\n", x);

	return 0;
}
