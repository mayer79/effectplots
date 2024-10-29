# Fix undefined global variable note in CRAN checks
utils::globalVariables(
  c("bin_center", "bin_width", "eval_at", "N", "varying_", "value_")
)
