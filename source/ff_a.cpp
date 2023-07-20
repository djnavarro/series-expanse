#include <Rcpp.h>
using namespace Rcpp;

// turmite function to be called from R
// [[Rcpp::export]]
NumericMatrix flame(int iter) {
  
  NumericMatrix points(iter, 3); // initially zero
  NumericMatrix coeffs(9, 4);
  
  // set coefficients
  for(int i = 0; i < 9; i++) {
    for(int j = 0; j < 4; j++) {
      coeffs(i,j) = R::runif(-1, 1);
    }
  }
  
  // initial values
  points(0, 0) = R::runif(-1, 1);
  points(0, 1) = R::runif(-1, 1);
  points(0, 2) = R::runif(-1, 1);
  
  // iterate
  int r;
  double x;
  double y;
  double z;
  double s;
  
  for(int t = 1; t < iter; t++) {
    
    r = rand() % 4; // which affine transform to use?
    
    // co-ordinates after random transform
    x = coeffs(0, r) * points(t-1, 0) + coeffs(1, r) * points(t-1, 1) + coeffs(2, r);
    y = coeffs(3, r) * points(t-1, 0) + coeffs(4, r) * points(t-1, 1) + coeffs(5, r);
    z = coeffs(6, r) * points(t-1, 0) + coeffs(7, r) * points(t-1, 1) + coeffs(8, r);
    
    // apply function to the transformed coords
    s = x*x + y*y + z*z;
    x = x/s;
    y = y/s;
    z = z/s;
    
    // store results
    points(t, 0) = x;
    points(t, 1) = y;
    points(t, 2) = (z + points(t-1, 2))/2;
    
  }
  
  return points;
}


