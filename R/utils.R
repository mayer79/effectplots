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

#' Calculate Plot Labels for Numeric Missing
#'
#' Internal function that calculates information needed for modifying a numeric x axis
#' to contain a value "NA".
#'
#' @noRd
#' @keywords internal
#'
#' @param x Object of class "EffectData"
#' @param plotly Plotly (`TRUE`) or not?
#' @returns
#'   A list with the numeric representation of the NA value, the vector of numeric
#'   breaks, and the vector of corresponding labels.
numeric_scale_with_na <- function(x, plotly) {
  # Note: the nrow(x) == 1 NA case was turned into character, and is thus
  # not possible at this point
  n <- nrow(x)
  mids <- x$bin_mid
  stopifnot(
    n >= 2L,
    is.na(mids[n]),
    is.numeric(mids)
  )
  r <- range(mids, na.rm = TRUE)
  breaks <- labeling::extended(r[1L], r[2L], m = min(4L, n - 1L))  # TODO find out plotly rule
  # TODO: reduce to unique values of mids if length(mids) <= 3?

  if (plotly) {
    # More or less the default scaler of plotly
    labeler <- scales::label_number(scale_cut = scales::cut_long_scale())
  } else {
    labeler <- scales::label_number_auto()
  }
  labels <- labeler(breaks)

  m <- length(breaks)
  min_gap <- sum(utils::tail(x$bin_width, 2)) * 0.6

  if (r[2L] + min_gap <= breaks[m]) {
    na.value <- breaks[m]
    labels[m] <- "NA"
  } else {
    if (length(breaks) == 1L) {
      D <- ceiling(min_gap)  # TODO: Better logic
      nbreaks <- 1
    } else {
      D <- diff(breaks)[m - 1L]
      nbreaks <- which(breaks[m] + D * (1:10) >= r[2L] + min_gap)[1L]
    }
    na.value <- breaks[m] + D * nbreaks
    breaks[m + 1L] <- na.value
    labels[m + 1L] <- "NA"
  }
  return(list(na.value = na.value, breaks = breaks, labels = labels))
}
