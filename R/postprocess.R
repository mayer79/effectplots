#' Postprocess Output
#'
#' This function offers different ways to improve the output of [marginal()],
#' [partial_dependence()], [average_observed()]. All arguments are vectorized, i.e.,
#' you can select different values per X variable.
#'
#' @param object Object of class "marginal".
#' @param eval_at_center If `FALSE` (default), the points are aligned with (weighted)
#'   average X values per bin. If `TRUE`, the points are aligned with bar centers.
#'   Since categoricals are always evaluated at bar centers, this only affects numerics.
#' @param drop_below_n Drop categories with exposure below this value.
#'   Only for categorical X variables.
#' @param drop_below_prop Drop categories with relative exposure below this value.
#'   Only for categorical X variables.
#' @param explicit_na Should `NA` levels be converted to strings?
#'   Only for categorical X variables.
#' @param na.rm Should `NA` levels in X be dropped?
#' @seealso [marginal()], [average_observed()], [partial_dependence()]
#' @export
#' @examples
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' xvars <- colnames(iris)[-1]
#' marginal(fit, v = xvars, data = iris, y = "Sepal.Length", breaks = 5) |>
#'   postprocess(eval_at_center = TRUE) |>
#'   plot()
postprocess <- function(
  object,
  eval_at_center = FALSE,
  drop_below_n = 0,
  drop_below_prop = 0,
  explicit_na = TRUE,
  na.rm = FALSE
) {
  stopifnot(inherits(object, "marginal"))

  out <- mapply(
    postprocess_one,
    x = object,
    eval_at_center = eval_at_center,
    drop_below_n = drop_below_n,
    drop_below_prop = drop_below_prop,
    explicit_na = explicit_na,
    na.rm = na.rm,
    SIMPLIFY = FALSE
  )
  class(out) <- "marginal"
  out
}


postprocess_one <- function(
  x,
  eval_at_center,
  drop_below_n,
  drop_below_prop,
  explicit_na,
  na.rm,
  ...
) {
  num <- is.numeric(x$eval_at)

  if (num) {
    if (isTRUE(eval_at_center)) {
      x$eval_at <- x$bar_at
    }
  }

  if (!num) {
    if (drop_below_n > 0) {
      x <- subset(x, exposure >= drop_below_n)
    }
    if (drop_below_prop > 0) {
      x <- subset(x, exposure / sum(exposure) >= drop_below_prop)
    }
    if (isTRUE(explicit_na)) {
      s <- is.na(x$bar_at)
      if (any(s)) {
        lvl <- levels(x$bar_at)
        if ("NA" %in% lvl) {
          stop("'bar_at' contains level 'NA'. This is incompatible with missing handling.")
        }
        levels(x$bar_at) <- levels(x$eval_at) <- c(lvl, "NA")
        x[s, c("bar_at", "eval_at")] <- "NA"
      }
    }
    x <- droplevels(x)
  }

  if (isTRUE(na.rm)) {
    x <- subset(x, !is.na(bar_at))
  }

  x
}
