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
#'   statistic (pd > pred_mean > y_mean > resid_mean). The default is `FALSE`.
#' @param sort_by If `sort = TRUE`, this measure used for sorting. One of
#'   'pd', 'pred_mean', 'y_mean', or 'resid_mean' (if available). The default is `NULL`,
#'   which picks the first of the listed statistics.
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
#' @seealso
#'   [marginal()], [average_observed()], [partial_dependence()],
#'   [main_effect_importance()]
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
  sort_by = NULL,
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
    imp <- main_effect_importance(out, by = sort_by)
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
#' (pd > pred_mean > y_mean > resid_mean) from a "marginal" object.
#' Serves as a simple measure of (main effect) importance. If partial dependence
#' is used, the measure is closely related to the importance measure proposed in
#' the reference below.
#'
#' @param x Marginal object.
#' @param by The statistic used to calculate the variance for.
#' One of 'pd', 'pred_mean', 'y_mean', or 'resid_mean`. By default (`NULL`), the
#' first available of above list.
#' @seealso [postprocess()]
#' @export
#' @inherit postprocess references
#' @examples
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' xvars <- colnames(iris)[-1]
#' M <- marginal(fit, v = xvars, data = iris, y = "Sepal.Length", breaks = 5)
#' main_effect_importance(M)
main_effect_importance <- function(x, by = NULL) {
  if (is.null(by)) {
    # "resid_mean" can never be picked if by = NULL, because there is also "pred_mean"
    vars <- intersect(c("pd", "pred_mean", "y_mean", "resid_mean"), colnames(x[[1L]]))
    by <- vars[1L]
    message("Importance via weighted variance of '", by, "'")
  }
  vapply(x, FUN = .one_imp, v = by, FUN.VALUE = numeric(1L))
}

# Helper functions
.one_imp <- function(x, v) {
  ok <- is.finite(x[[v]])
  stats::cov.wt(x[ok, v, drop = FALSE], wt = x$weight[ok], method = "ML")$cov[1L, 1L]
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
  w <- x_agg$weight
  x_new <- data.frame(
    bin_mid = oth, bin_width = 0.7, bin_mean = oth, N = sum(x_agg$N), weight = sum(w)
  )

  m_cols <- intersect(colnames(x), c("pred_mean", "y_mean", "resid_mean", "pd"))
  if (length(m_cols)) {
    x_new[, m_cols] <- collapse::fmean(x_agg[m_cols], w = w, drop = FALSE)
  }

  s_cols <- intersect(colnames(x), c("pred_sd", "y_sd", "resid_sd"))
  if (length(s_cols)) {
    # Bins with N = 1 can be NA, therefore na.rm = TRUE
    x_new[, s_cols] <- sqrt(
      collapse::fmean(x_agg[s_cols]^2, w = w, drop = FALSE, na.rm = TRUE)
    )
  }
  rbind(x_keep, x_new)  # Column order of x_new does not matter
}
