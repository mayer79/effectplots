#' Bias / Average Residuals
#'
#' Calculates average residuals (= bias) over the values of one or multiple
#' features specified by `X`.
#'
#' The function is a convenience wrapper around [feature_effects()].
#'
#' @param resid A numeric vector of residuals, i.e., y - pred.
#' @inheritParams average_observed
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
    discrete_m = 13L,
    outlier_iqr = 2,
    seed = NULL,
    ...
) {
  if (NCOL(X) == 1L && (is.vector(X) || is.factor(X))) {
    X <- collapse::frename(collapse::qDF(X), x_name)
  }
  stopifnot(
    is.matrix(X) || is.data.frame(X),
    length(resid) == nrow(X),
    is.null(w) || length(w) == nrow(X)
  )

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
    ale_n = 0L,
    seed = seed
  )
  nms <- colnames(out[[1L]])
  p <- length(nms)
  nms[(p - 1L):p] <- c("resid_mean", "resid_sd")
  out[] <- lapply(out, function(z) stats::setNames(z, nms))
  out
}

