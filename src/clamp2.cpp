#include <Rcpp.h>
using namespace Rcpp;

//' Clamps/clips Vector
//'
//' Much faster than `pmax(pmin, x, high), low)`.
//'
//' @noRd
//' @keywords internal
//' @param x Numeric vector to be clamped/clipped.
//' @param low Lowest value.
//' @param low Highest value.
//' @return Clamped version of x.
// [[Rcpp::export]]
NumericVector clamp2(NumericVector x, double low, double high) {
  for (int i = 0; i < x.size(); i++) {
    if (x[i] > high) {
      x[i] = high;
    } else if (x[i] < low) {
      x[i] = low;
    }
  }
  return x;
}

