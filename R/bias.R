#' Bias / Average Residuals
#'
#' Calculates average residuals (= bias) over the values of one or multiple
#' `X` variables.
#'
#' The function is a convenience wrapper around [feature_effects()].
#'
#' @param X A vector, matrix, or data.frame with variable(s) to be shown on the x axis.
#' @param resid A numeric vector of residuals, i.e., y - pred.
#' @param w An optional numeric vector of weights.
#' @param x_name If `X` is a vector: what is the name of the variable? By default "x".
#' @inheritParams feature_effects
#' @inherit feature_effects return
#' @param ... Currently unused.
#' @seealso [feature_effects()]
#' @export
#' @examples
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' M <- bias(iris[2:5], resid = fit$residuals, breaks = 5)
#' M |> update(sort_by = "resid_mean") |> plot(share_y = "all")
bias <- function(
    X,
    resid,
    w = NULL,
    x_name = "x",
    breaks = "Sturges",
    right = TRUE,
    discrete_m = 5L,
    outlier_iqr = 2,
    ...
) {
  if (NCOL(X) == 1L && (is.vector(X) || is.factor(X))) {
    X <- collapse::frename(collapse::qDF(X), x_name)
  }

  # We treat "resid" as "y" and then change y_mean/sd to resid_mean/sd
  out <- feature_effects.default(
    object = NULL,
    v = colnames(X),
    data = X,
    y = resid,
    w = w,
    breaks = breaks,
    right = right,
    discrete_m = discrete_m,
    outlier_iqr = outlier_iqr,
    calc_pred = FALSE,
    pd_n = 0L,
    ale_bin_size = 0L
  )
  nms <- colnames(out[[1L]])
  p <- length(nms)
  nms[(p - 1L):p] <- c("resid_mean", "resid_sd")
  out[] <- lapply(out, function(z) stats::setNames(z, nms))
  out
}

