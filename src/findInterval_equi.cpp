#include <Rcpp.h>
using namespace Rcpp;

//' findInterval() for equi-length intervals
//'
//' A fast version of collapse::qF(findInterval(, all.inside = TRUE), explicit.na = T/F)
//' that works for equi-length intervals. Set `explicit_na = true` for maximal speed
//' when combined with fast aggregation in "collapse".
//'
//' @noRd
//' @keywords internal
//' @param x Numeric vector to be binned.
//' @param low Lowest value.
//' @param low Highest value.
//' @param nbin Number of bins (= length(breaks) - 1L).
//' @param labels A string vector of labels for the bins. Must be passed even if
//'   `codes_only = true`.
//' @param right Right-closed intervals? Default is `true`.
//' @param explicit_na Should missing values be coded as nbin + 1? If `true`,
//'   the resulting factor will get an explicit level `NA`, and an additional class
//'   `"na.included"`.
//' @param codes_only Should only integer codes be returned? Default is `false`.
//' @return
//'   Either an integer vector or a factor representing the bins.
// [[Rcpp::export]]

IntegerVector findInterval_equi(
    NumericVector x,
    double low,
    double high,
    int nbin,
    StringVector labels,
    bool right = true,
    bool explicit_na = false,
    bool codes_only = false

) {
  const int n = x.size();
  const double D = (high - low) / nbin;
  int na_count = 0;
  int na_value = explicit_na ? nbin + 1 : NA_INTEGER;
  double z;
  IntegerVector out(n, na_value);

  for (int i = 0; i < n; i++) {
    z = x[i];
    if (z <= low) {
      out[i] = 1;
    } else if (z >= high) {
      out[i] = nbin;
    } else if (std::isnan(z)) {
      na_count++;
    } else {
      out[i] = right ? int(ceil((z - low) / D)) : int((z - low) / D) + 1;
    }
  }
  if (codes_only) {
    return out;
  }

  StringVector cl = {"factor"};
  if (explicit_na) {
    cl.push_back("na.included");
    if (na_count > 0) {
      labels.push_back(NA_STRING);
    }
  }
  out.attr("class") = cl;
  out.attr("levels") = labels;

  return out;
}

