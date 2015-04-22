#include "armalib.h"

int main(int argc, char **argv)
{
	// calib_init HAS TO be always the first
	calib_init(argc, argv);

	arma::mat A;
	  A << 1.0 << 2.0 << arma::endr
	    << 2.0 << 3.0 << arma::endr
	    << 1.0 << 3.0 << arma::endr;		
	arma::mat B;
	  B << 1.0 << 2.0 << 7.0 << arma::endr
	    << 2.0 << 0.0 << 3.0 << arma::endr;	    
	
	arma::mat C;
	armamult(A, B, C);

	return 0;
}
