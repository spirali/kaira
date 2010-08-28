
#include <stdio.h>
#include "cailie.h"

void my_main(CaContext *ctx) {
	printf("Context %p\n", ctx);
	printf("Node %i\n", ctx->node());
}

int main() {
	printf("Cailie Test\n");
	ca_main(2, my_main);
	return 0;
}
