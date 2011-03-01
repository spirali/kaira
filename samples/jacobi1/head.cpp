
#include <iostream>
#include <fstream>

int parameter_SIZE_X();
int parameter_SIZE_Y();
int parameter_TEMP();
//int parameter_BORDER_TEMP();


void iid_to_pos(int iid, int &x, int &y)
{
	y = iid / parameter_SIZE_X();
	x = iid % parameter_SIZE_X();
}

int pos_to_iid(int x, int y)
{
	return x + (y * parameter_SIZE_X());
}


double initial_content(int x, int y)
{
	if (x == parameter_SIZE_X() / 2 && y == parameter_SIZE_Y() / 2) {
		return parameter_TEMP();
	} else {
		return 0.0;
	}
}