# Fix undefined global variable note in CRAN checks
utils::globalVariables(
  c("bin_mid", "bin_width", "bin_mean", "N", "weight", "size", "varying_", "value_", "err_")
)
