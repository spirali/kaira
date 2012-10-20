#include "simplemodule.h"

int main(int argc, char **argv)
{
	calib_init(argc, argv);

	for (int s = 0; s < 128; s++) {
		int t = 0;
		while (t < 1000) {
			simplemodule(t);
		}
	}

	return 0;
}
