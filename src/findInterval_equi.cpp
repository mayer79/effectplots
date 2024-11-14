#include <Rcpp.h>
using namespace Rcpp;

//' findInterval() for equi-length intervals
//'
//' Results agree with `findInterval(..., rightmost.closed = TRUE, all.inside = TRUE)`.
//'
//' @noRd
//' @keywords internal
//' @param x Numeric vector to be binned.
//' @param low Lowest value.
//' @param low Highest value.
//' @param nbin Number of bins (= length(breaks) - 1L).
//' @param right Right-closed intervals? Default is `true`.
//' @return Binned version of `x` (integer encoded).
// [[Rcpp::export]]
IntegerVector findInterval_equi(
    NumericVector x, double low, double high, int nbin, bool right = true
) {
  const int n = x.size();
  IntegerVector out(n, NA_INTEGER);
  double D = (high - low) / nbin;

  for (int i = 0; i < n; i++) {
    double z = x[i];
    if (z <= low) {
      out[i] = 1;
    } else if (z >= high) {
      out[i] = nbin;
    } else if (!std::isnan(z)) {
      if (right) {
        out[i] = int(ceil((z - low) / D));
      } else {
        out[i] = int((z - low) / D) + 1;
      }
    }
  }
  return out;
}

