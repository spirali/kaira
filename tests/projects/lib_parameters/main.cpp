#include <stdio.h>
#include <vector>
#include "parameters.h"

int main(int argc, char **argv) {

	calib_init(argc, argv);

	int x = 2;
	std::vector<int > v (4,1);
	std::vector<int > w;
	for(int i = 0 ; i < 5 ; i++) {
		set_parameter_EXP(i);
		set_parameter_Size(i);
		parameters(x, v, w);
		for (int i = 0; i < 4; i++) {
			printf("%d ", w[i]);
		}
		printf("\n");
	}

	return 0;
}
