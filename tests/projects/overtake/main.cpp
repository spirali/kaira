#include <stdio.h>
#include "overtake.h"

int main(int argc, char **argv) {

	calib_init(argc, argv);

	int x = 0;
	for(int i = 0 ; i < 100 ; i++)
	{
		overtake(x);
		printf("%d\n", x);
	}

	return 0;
}
