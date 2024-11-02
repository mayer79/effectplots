#' Variable Importance
#'
#' @description
#' Extracts from a "marginal" object a simple variable importance measure, namely
#' the (bin size weighted) variance of the partial dependence values, or of any other
#' calculated statistic (e.g., "pred_mean" or "y_mean"). It can be used via
#' `update.marginal(, sort_by = "pd")` to sort the variables in decreasing importance.
#' Note that this measure captures only the main effect strength.
#' If the importance is calculated with respect to "pd", it is closely related
#' to the suggestion of Greenwell et al. (2018).
#'
#' @param x Object of class "marginal".
#' @param by The statistic used to calculate the variance for.
#' One of 'pd', 'pred_mean', 'y_mean', 'resid_mean', or 'ale' (if available).
#' The default is `NULL`, which picks the first available statistic from above list.
#' @seealso [update.marginal()]
#' @export
#' @references
#'   Greenwell, Brandon M., Bradley C. Boehmke, and Andrew J. McCarthy. 2018.
#'     *A Simple and Effective Model-Based Variable Importance Measure.*
#'     arXiv preprint. <https://arxiv.org/abs/1805.04755>.
#' @examples
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' M <- marginal(fit, v = colnames(iris)[-1], data = iris)
#' ep_importance(M)
ep_importance <- function(x, by = NULL) {
  stopifnot(inherits(x, "marginal"))
  S <- c("pd", "pred_mean", "y_mean", "resid_mean", "ale")
  if (is.null(by)) {
    by <- intersect(S, colnames(x[[1L]]))[1L]
    message("Importance via weighted variance of '", by, "'")
  } else {
    stopifnot(by %in% S)
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
