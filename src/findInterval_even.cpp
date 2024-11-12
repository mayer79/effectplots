#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector findInterval_even(
    NumericVector x, double low, double high, int nbin, bool right = true
) {
  int n = x.size();
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
