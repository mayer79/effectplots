#' Is "EffectData" Discrete
#'
#' Internal function that shows which of the list elements in an object `x` of class
#' "EffectData" is to be considered as numeric or not.
#'
#' @noRd
#' @keywords internal
#'
#' @param x An object of class "EffectData".
#' @returns
#'   A logical vector of the same length as `x` with the information whether the
#'   feature of `x` is to be treated as numeric or not.
is_discrete <- function(x) {
  f <- function(z) attr(z, "discrete")
  if (inherits(x, "EffectData")) {
    return(vapply(x, FUN = f, FUN.VALUE = logical(1L), USE.NAMES = FALSE))
  }
  f(x)
}

#' Get Statistics from "EffectData" Object
#'
#' Internal function that returns a character vector of statistics present in `x`.
#' Subset of `c("pred_mean", "y_mean", "resid_mean", "pd", "ale")`.
#'
#' @noRd
#' @keywords internal
#'
#' @param x An object of class "EffectData".
#' @returns
#'   A character vector of statistics names present in `x`.
.stats <- function(x) {
  if (inherits(x, "EffectData")) {
    x <- x[[1L]]
  }
  statistics <- c("pred_mean", "y_mean", "resid_mean", "pd", "ale")
  return(intersect(statistics, colnames(x)))
}

#' Converts Rownames to Original Type
#'
#' Internal function used in `calculate_stats()` to turn rownames `x` of a grouped
#' aggregation data back to the original `type`. For doubles, we can lose precision,
#' but this should not a problem here.
#'
#' @noRd
#' @keywords internal
#'
#' @param x A character vector of row names, may contain `NA`.
#' @param type One of "factor", "double", "integer", "logical", and "character".
#' @param ord Is the factor ordered? By default `FALSE`. Only relevant for factors.
#' @param lev If type = "factor", its original levels.
#' @returns
#'   A data.frame with variables not in `to_stack`, a column "varying_" with
#'   the column name from `to_stack`, and finally a column "value_" with stacked values.
parse_rownames <- function(x, type, ord = FALSE, lev = NULL) {
  if (!is.character(x)) {
    stop("Row names must be strings")
  }
  if (type == "factor" && is.null(lev)) {
    stop("Need 'lev' for type = 'factor'.")
  }
  switch(
    type,
    factor = factor(x, levels = lev, ordered = ord),
    double = as.double(x),
    integer = as.integer(x),
    logical = as.logical(x),
    character = x,
    stop("Can only handle features of type factor, double, integer, logical, and character.")
  )
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
    average_margin <- margin * (m - 2) / m
    outer_size <- 1 / m - average_margin
    inner_size <- 1 / m - average_margin + margin
    return(c(outer_size, rep(inner_size, m - 2L), outer_size))
  }
  NULL
}

get_ylab <- function(lines) {
  if (length(lines) == 1L) {
    out <- switch(
      lines,
      pred_mean = "Average prediction",
      y_mean = "Average response",
      resid_mean = "Bias",
      pd = "Partial Dependence",
      ale = "Accumulated local effects"
    )
    return(out)
  }
  return("Effect")
}

common_range <- function(x, stat_info) {
  r <- range(sapply(x, function(z) range(z[stat_info], na.rm = TRUE)))
  return(grDevices::extendrange(r, f = 0.05))
}
