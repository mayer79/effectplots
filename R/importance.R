#' Variable Importance
#'
#' @description
#' Extracts from an "EffectData" object a simple variable importance measure, namely
#' the (bin size weighted) variance of the partial dependence values, or of any other
#' calculated statistic (e.g., "pred_mean" or "y_mean"). It can be used via
#' `update.EffectData(, sort_by = "pd")` to sort the variables in decreasing importance.
#' Note that this measure captures only the main effect strength.
#' If the importance is calculated with respect to "pd", it is closely related
#' to the suggestion of Greenwell et al. (2018).
#'
#' @param x Object of class "EffectData".
#' @param by The statistic used to calculate the variance for.
#' One of 'pd', 'pred_mean', 'y_mean', 'resid_mean', or 'ale' (if available).
#' The default is `NULL`, which picks the first available statistic from above list.
#' @returns A named vector of importance values of the same length as `x`.
#' @seealso [update.EffectData()]
#' @export
#' @references
#'   Greenwell, Brandon M., Bradley C. Boehmke, and Andrew J. McCarthy. 2018.
#'     *A Simple and Effective Model-Based Variable Importance Measure.*
#'     arXiv preprint. <https://arxiv.org/abs/1805.04755>.
#' @examples
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' M <- feature_effects(fit, v = colnames(iris)[-1], data = iris)
#' effect_importance(M)
effect_importance <- function(x, by = NULL) {
  stopifnot(inherits(x, "EffectData"))

  # Different order as .stats(x)
  S <- intersect(c("pd", "pred_mean", "y_mean", "resid_mean", "ale"), .stats(x))
  if (is.null(by)) {
    by <- S[1L]
    message("Importance via weighted variance of '", by, "'")
  } else if (!(by %in% S)) {
    stop(paste("'by' must be in", paste(sQuote(S), collapse = ", ")))
  }
  return(vapply(x, FUN = .one_imp, by = by, FUN.VALUE = numeric(1L)))
}

# Helper function
.one_imp <- function(x, by) {
  ok <- is.finite(x[[by]])
  if (sum(ok) < 2L) {
    return(0)
  }
  stats::cov.wt(x[ok, by, drop = FALSE], wt = x$weight[ok], method = "ML")$cov[1L, 1L]
}
