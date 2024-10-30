#' Postprocess Marginal Object
#'
#' This function helps to improve the output of [marginal()],
#' [partial_dependence()], and [average_observed()].
#'
#' @details
#' If `sort = TRUE`, the features will be sorted by a simple variable importance
#' measure. It is calculated as the variance of the most relevant available statistic
#' (pd > pred_mean > y_mean). The variance is weighted with bin weights to reflect
#' the data distribution. For "pd", this measure is similar (but not identical) to the
#' suggestion of Greenwell et al. (2018).
#' Importance is calculated after the other postprocessings, e.g., after collapsing
#' rare levels.
#'
#' Except for `sort`, all arguments are vectorized, i.e., you can
#' pass a vector or list of the same length as `x`.
#'
#' @param x Object of class "marginal".
#' @param sort Should `x` be sorted in decreasing order of feature importance?
#'   Importance is measured by the weighted variance of the most relevant available
#'   statistic (pd > pred_mean > y_mean), see Details. The default is `FALSE`.
#' @param collapse_m If a categorical X has more than `collapse_m` levels,
#'   rare levels are collapsed into a new level "Other". Standard deviations are
#'   collapsed via root of the weighted average variances. By default 30.
#'   Set to `Inf` for no collapsing. Vectorized over `x`.
#' @param collapse_by How to determine "rare" levels in `collapse_m`?
#'   Either "weight" (default) or "N". Only matters in situations with case weights `w`.
#' @param drop_below_n Drop bins with N below this value. Applied after collapsing.
#'   Vectorized over `x`.
#' @param drop_below_weight Drop bins with weight below this value. Applied after
#' collapsing. Vectorized over `x`.
#' @param na.rm Should missing bin centers be dropped? Default is `FALSE`.
#' @seealso [marginal()], [average_observed()], [partial_dependence()]
#' @export
#' @examples
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' xvars <- colnames(iris)[-1]
#' marginal(fit, v = xvars, data = iris, y = "Sepal.Length", breaks = 5) |>
#'   postprocess(sort = TRUE) |>
#'   plot()
#' @references
#'   Greenwell, Brandon M., Bradley C. Boehmke, and Andrew J. McCarthy.
#'   *A Simple and Effective Model-Based Variable Importance Measure.* Arxiv (2018).
postprocess <- function(
  x,
  sort = FALSE,
  collapse_m = 30L,
  collapse_by = c("weight", "N"),
  drop_below_n = 0,
  drop_below_weight = 0,
  na.rm = FALSE
) {
  stopifnot(
    inherits(x, "marginal"),
    collapse_m >= 2L
  )
  collapse_by <- match.arg(collapse_by)

  out <- mapply(
    postprocess_one,
    x = x,
    collapse_m = collapse_m,
    collapse_by = collapse_by,
    drop_below_n = drop_below_n,
    drop_below_weight = drop_below_weight,
    na.rm = na.rm,
    SIMPLIFY = FALSE
  )

  if (isTRUE(sort)) {
    imp <- main_effect_importance(out)
    out <- out[order(imp, decreasing = TRUE, na.last = TRUE)]
  }

  return(structure(out, class = "marginal"))
}

postprocess_one <- function(
    x,
    collapse_m,
    collapse_by,
    drop_below_n,
    drop_below_weight,
    na.rm
) {
  if (!.num(x) && collapse_m < nrow(x)) {
    x <- .collapse_m(x, m = collapse_m, by = collapse_by)
  }
  if (drop_below_n > 0) {
    x <- subset(x, N >= drop_below_n)
  }
  if (drop_below_weight > 0) {
    x <- subset(x, weight >= drop_below_weight)
  }
  if (isTRUE(na.rm)) {
    x <- subset(x, !is.na(bin_mid))
  }
  return(droplevels(x))
}

#' Main Effect Importance
#'
#' Extracts the weighted variances of the most relevant statistic
#' (pd > pred_mean > y_mean) from a "marginal" object.
#' Serves as a simple "main effect" importance measure.
#'
#' @param x Marginal object.
#' @param statistic The statistic used to calculate the variance for.
#' One of 'pd', 'pred_mean', or 'y_mean'.
#' @seealso [postprocess()]
#' @export
#' @examples
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' xvars <- colnames(iris)[-1]
#' M <- marginal(fit, v = xvars, data = iris, y = "Sepal.Length", breaks = 5)
#' main_effect_importance(M)
main_effect_importance <- function(x, statistic = NULL) {
  if (is.null(statistic)) {
    vars <- intersect(c("pd", "pred_mean", "y_mean"), colnames(x[[1L]]))
    statistic <- vars[1L]
    message("Importance via weighted variance of '", statistic, "'")
  }
  vapply(x, FUN = .one_imp, v = statistic, FUN.VALUE = numeric(1L))
}

# Helper functions
.one_imp <- function(x, v) {
  ok <- is.finite(x[[v]])
  stats::cov.wt(x[ok, v, drop = FALSE], x$weight[ok], method = "ML")$cov[1L, 1L]
}

.collapse_m <- function(x, m, by) {
  x_list <- split(x, order(x[[by]], decreasing = TRUE) < m)
  x_keep <- x_list$`TRUE`
  x_agg <- x_list$`FALSE`

  # Prepare new factors
  x_keep <- droplevels(x_keep)
  lvl <- levels(x_keep$bin_mid)
  oth <- make.names(c(lvl, "other"), unique = TRUE)[length(lvl) + 1L]
  levels(x_keep$bin_mid) <- levels(x_keep$bin_mean) <- c(lvl, oth)

  # Collapse other rows
  M <- x_agg[intersect(colnames(x), c("pred_mean", "y_mean", "pd"))]
  S <- x_agg[intersect(colnames(x), c("pred_sd", "y_sd"))]
  w <- x_agg$weight
  x_new <- data.frame(
    bin_mid = oth, bin_width = 0.7, bin_mean = oth, N = sum(x_agg$N), weight = sum(w)
  )
  if (NCOL(M)) {
    x_new[, colnames(M)] <- collapse::fmean(M, w = w, drop = FALSE)
  }
  if (NCOL(S)) {
    x_new[, colnames(S)] <- sqrt(
      collapse::fmean(S^2, w = w, drop = FALSE, na.rm = TRUE)  # Bins with N=1 can be NA
    )
  }
  rbind(x_keep, x_new)  # Column order of x_new does not matter
}
