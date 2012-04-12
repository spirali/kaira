
#include <stdio.h>
#include "libhelloworld.h"

int main(int argc, char **argv)
{
	calib_init(argc, argv);
	int x = 20, y = 10;
	std::string s;

	say_hello(x, y, s);
	printf("%i %i %s\n", x, y, s.c_str());
	say_hello(x, y, s);
	printf("%i %i %s\n", x, y, s.c_str());
	say_hello(x, y, s);
	printf("%i %i %s\n", x, y, s.c_str());
	say_hello(x, y, s);
	printf("%i %i %s\n", x, y, s.c_str());
	say_hello(x, y, s);
	printf("%i %i %s\n", x, y, s.c_str());
}
