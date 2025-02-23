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
  f <- function(z) {
    out <- attr(z, "discrete")
    if (is.null(out)) {
      stop("Attribute 'discrete' not found.")
    }
    return(out)
  }
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
    factor = factor(
      x, levels = lev, ordered = ord, exclude = if (anyNA(lev)) NULL else NA
    ),
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

#' Calculate Plot Labels for Numeric Missing
#'
#' Internal function that calculates information needed for modifying a numeric x axis
#' to contain a value "NA". This function can be improved. Note that the result is
#' built consistently around ggplot2, not plotly.
#'
#' @noRd
#' @keywords internal
#'
#' @param mids Bin mid values.
#' @param widths Bin widths.
#' @param plotly Logical flag whether plotly's or ggplot's labeler is to be used.
#' @returns
#'   A list with the numeric representation of the NA value, the vector of numeric
#'   breaks, and the vector of corresponding labels.
numeric_scale_with_na <- function(mids, widths, plotly) {
  n <- length(mids)
  stopifnot(
    n == length(widths),
    is.na(mids[n]),
    is.numeric(mids)
  )

  # Easy: the only bin is the NA bin
  if (n == 1L) {
    return(list(na.value = 0, breaks = 0, labels = "NA"))
  }

  # If no bars are to be plotted, we could even narrow the range further down
  r <- mids[c(1L, n - 1L)] + c(-1, 1) * widths[c(1L, n - 1L)] / 2
  breaks <- labeling::extended(r[1L], r[2L], m = max(2L, min(5L, n - 1L)))
  m <- length(breaks)  # I hope that this has length >= 2

  # Let's mimic the defaults
  if (plotly) {
    labeler <- scales::label_number(scale_cut = scales::cut_long_scale())
  } else {
    labeler <- scales::label_number_auto()
  }
  labels <- labeler(breaks)

  # Minimal gap between r[2] and the midpoint of the NA bin
  min_gap <- widths[n] * 0.7

  # The last break is so far away that it can play the role of the NA break
  if (r[2L] + min_gap <= breaks[m]) {
    labels[m] <- "NA"
    return(list(na.value = breaks[m], breaks = breaks, labels = labels))
  }

  # How many additional breaks would we need to cover min_gap?
  D <- diff(breaks)[1L]  # All diffs are equal
  additional_breaks <- which(breaks[m] + (1:10) * D >= r[2L] + min_gap)[1L]

  na.value <- breaks[m] + additional_breaks * D
  breaks[m + 1L] <- na.value
  labels[m + 1L] <- "NA"

  return(list(na.value = na.value, breaks = breaks, labels = labels))
}
