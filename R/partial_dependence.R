#' Partial Dependence
#'
#' Calculates partial dependence of one or more X variables (`v`) in `data`.
#' This function is a convenience wrapper around [marginal()].
#'
#' @inheritParams marginal
#' @inherit marginal return
#' @references
#'   Friedman, Jerome H. 2001, *Greedy Function Approximation: A Gradient Boosting Machine.*
#'     Annals of Statistics 29 (5): 1189-1232. doi:10.1214/aos/1013203451.
#' @seealso [marginal()]
#' @export
#' @examples
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' M <- partial_dependence(fit, v = "Species", data = iris)
#' M |> plot()
#'
#' M2 <- partial_dependence(fit, v = colnames(iris)[-1], data = iris)
#' plot(M2, share_y = TRUE)
partial_dependence <- function(object, ...) {
  UseMethod("partial_dependence")
}

#' @describeIn partial_dependence Default method.
#' @export
partial_dependence.default <- function(
    object,
    v,
    data,
    pred_fun = stats::predict,
    trafo = NULL,
    which_pred = NULL,
    w = NULL,
    breaks = "Sturges",
    right = TRUE,
    discrete_m = 5L,
    outlier_iqr = 2,
    pd_n = 500L,
    ...
) {
  marginal.default(
    object = object,
    v = v,
    data = data,
    y = NULL,
    pred_fun = pred_fun,
    trafo = trafo,
    which_pred = which_pred,
    w = w,
    breaks = breaks,
    right = right,
    discrete_m = discrete_m,
    outlier_iqr = outlier_iqr,
    calc_pred = FALSE,
    pd_n = pd_n,
    ...
  )
}

#' @describeIn partial_dependence Default method.
#' @export
partial_dependence.ranger <- function(
    object,
    v,
    data,
    pred_fun = NULL,
    trafo = NULL,
    which_pred = NULL,
    w = NULL,
    breaks = "Sturges",
    right = TRUE,
    discrete_m = 5L,
    outlier_iqr = 2,
    pd_n = 500L,
    ...
) {
  if (is.null(pred_fun)) {
    pred_fun <- function(model, newdata, ...) {
      stats::predict(model, newdata, ...)$predictions
    }
  }
  partial_dependence.default(
    object = object,
    v = v,
    data = data,
    pred_fun = pred_fun,
    trafo = trafo,
    which_pred = which_pred,
    w = w,
    breaks = breaks,
    right = right,
    discrete_m = discrete_m,
    outlier_iqr = outlier_iqr,
    pd_n = pd_n,
    ...
  )
}

#' @describeIn partial_dependence Default method.
#' @export
partial_dependence.explainer <- function(
    object,
    v,
    data = object[["data"]],
    pred_fun = object[["predict_function"]],
    trafo = NULL,
    which_pred = NULL,
    w = object[["weights"]],
    breaks = "Sturges",
    right = TRUE,
    discrete_m = 5L,
    outlier_iqr = 2,
    pd_n = 500L,
    ...
) {
  partial_dependence.default(
    object = object[["model"]],
    v = v,
    data = data,
    pred_fun = pred_fun,
    trafo = trafo,
    which_pred = which_pred,
    w = w,
    breaks = breaks,
    right = right,
    discrete_m = discrete_m,
    outlier_iqr = outlier_iqr,
    pd_n = pd_n,
    ...
  )
}
