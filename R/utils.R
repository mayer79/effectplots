.num <- function(x) {
  f <- function(z) is.numeric(z$bin_mean)
  if (inherits(x, "marginal")) {
    return(vapply(x, FUN = f, FUN.VALUE = logical(1L), USE.NAMES = FALSE))
  }
  f(x)
}

# -> plot()

#' Stack some Columns (from hstats)
#'
#' Internal function used in the plot method for "pd" objects. The function brings
#' wide columns `to_stack` (the prediction dimensions) into long form.
#'
#' @noRd
#' @keywords internal
#'
#' @param data A data.frame.
#' @param to_stack Column names in `data` to bring from wide to long form.
#' @returns
#'   A data.frame with variables not in `to_stack`, a column "varying_" with
#'   the column name from `to_stack`, and finally a column "value_" with stacked values.
poor_man_stack <- function(data, to_stack) {
  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame.")
  }
  keep <- setdiff(colnames(data), to_stack)
  out <- lapply(
    to_stack,
    FUN = function(z) cbind.data.frame(data[keep], varying_ = z, value_ = data[, z])
  )
  out <- do.call(rbind, out)
  transform(out, varying_ = factor(varying_, levels = to_stack))
}

# subplots make inner plots smaller due to margins
# https://github.com/plotly/plotly.R/issues/2144
# We apply an approximate correction factor via heights and widths
# The function assumes symmetric margin pairs ((left, right), (top, bottom))
corr_margin <- function(m, margin) {
  if (m >= 3L) {
    f <- 1 / m * (1 - 2 * margin)  # ok? Or rather 1 / m / (1 + 2 * margin)?
    n_inner <- m - 2L
    return(c(f, rep((1 - 2 * f) / n_inner, times = n_inner), f))
  }
  return(NULL)
}

get_ylab <- function(lines) {
  if (length(lines) == 1L) {
    out <- switch(
      lines,
      y_mean = "Average response",
      pred_mean = "Average prediction",
      resid_mean = "Bias",
      pd = "Partial Dependence",
      ale = "Accumulated local effects"
    )
    return(out)
  }
  # No "average" (that would be overly specific)
  if ("y_mean" %in% lines) "Response" else "Prediction"
}

common_range <- function(x, stat_info) {
  r <- range(sapply(x, function(z) range(z[stat_info], na.rm = TRUE)))
  return(grDevices::extendrange(r, f = 0.05))
}
