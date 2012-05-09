
#include <stdio.h>
#include "rpc.h"

int main(int argc, char **argv)
{
	calib_init(argc, argv);

	MyClass m;
	m.x = 10;
	m.y = 33;

	int x = 20;
	int y = 30;
	int z = 40;

	fn1(x, y, z);
	fn1(x, y, z);
	fn2(z, m, y);
	fn2(z, m, z);

	printf("%i %i %i %i %i\n", x, y, z, m.x, m.y);
	return 0;
}
