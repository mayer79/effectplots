#include <Rcpp.h>
using namespace Rcpp;

//' findInterval() for equi-length intervals
//'
//' Returns a factor with explicit missing level.
//'
//' @noRd
//' @keywords internal
//' @param x Numeric vector to be binned.
//' @param low Lowest value.
//' @param low Highest value.
//' @param nbin Number of bins (= length(breaks) - 1L).
//' @param right Right-closed intervals? Default is `true`.
//' @return A factor representing a binned version of `x`. In line with collapse,
//'   missing values are encoded internally by an explicit level "NA" to allow for
//'   zero-copy grouped stats.
// [[Rcpp::export]]
IntegerVector findInterval_equi(
    NumericVector x, double low, double high, int nbin, bool right = true
) {
  const int n = x.size();
  const double D = (high - low) / nbin;
  int na_count = 0;
  IntegerVector out(n, nbin + 1);

  for (int i = 0; i < n; i++) {
    double z = x[i];
    if (z <= low) {
      out[i] = 1;
    } else if (z >= high) {
      out[i] = nbin;
    } else if (std::isnan(z)) {
      na_count++;
    } else {
      if (right) {
        out[i] = int(ceil((z - low) / D));
      } else {
        out[i] = int((z - low) / D) + 1;
      }
    }
  }

  Rcpp::StringVector classes = {"factor", "na.included"};
  Rcpp::StringVector levels (nbin);
  for (int i = 0; i < nbin; i++) {
    levels[i] = std::to_string(i + 1);
  }
  if (na_count > 0) {
    levels.push_back(NA_STRING);
  }
  out.attr("class") = classes;
  out.attr("levels") = levels;
  return out;
}

