#' Partial Dependence
#'
#' Calculates partial dependence of one or more variables (`x_name`) in
#' a dataset `data`. This function is a convenience wrapper over [marginal()].
#'
#' @inheritParams marginal
#' @export
#' @examples
#' library(ranger)
#'
#' fit <- ranger(Sepal.Length ~ ., data = iris)
#' M <- partial_dependence(fit, x_name = "Species", data = iris)
#' M
#' M |> plot()
partial_dependence <- function(object, ...) {
  UseMethod("partial_dependence")
}

#' @describeIn partial_dependence Default method.
#' @export
partial_dependence.default <- function(
    object,
    x_name,
    data,
    pred_fun = stats::predict,
    w = NULL,
    breaks = "Sturges",
    right = TRUE,
    discrete_m = 2L,
    winsorize_low = 0.01,
    winsorize_high = 0.99,
    pd_n = 500L,
    ...
) {
  marginal.default(
    object = object,
    x_name = x_name,
    data = data,
    y = NULL,
    pred_fun = pred_fun,
    w = w,
    breaks = breaks,
    right = right,
    discrete_m = discrete_m,
    winsorize_low = winsorize_low,
    winsorize_high = winsorize_high,
    calc_pred = FALSE,
    pd_n = pd_n,
    ...
  )
}

#' @describeIn partial_dependence Default method.
#' @export
partial_dependence.ranger <- function(
    object,
    x_name,
    data,
    pred_fun = NULL,
    w = NULL,
    breaks = "Sturges",
    right = TRUE,
    discrete_m = 2L,
    winsorize_low = 0.01,
    winsorize_high = 0.99,
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
    x_name = x_name,
    data = data,
    pred_fun = pred_fun,
    w = w,
    breaks = breaks,
    right = right,
    discrete_m = discrete_m,
    winsorize_low = winsorize_low,
    winsorize_high = winsorize_high,
    pd_n = pd_n,
    ...
  )
}

#' @describeIn partial_dependence Default method.
#' @export
partial_dependence.explainer <- function(
    object,
    x_name,
    data = object[["data"]],
    pred_fun = object[["predict_function"]],
    w = object[["weights"]],
    breaks = "Sturges",
    right = TRUE,
    discrete_m = 2L,
    winsorize_low = 0.01,
    winsorize_high = 0.99,
    pd_n = 500L,
    ...
) {
  partial_dependence.default(
    object = object[["model"]],
    x_name = x_name,
    data = data,
    pred_fun = pred_fun,
    w = w,
    breaks = breaks,
    right = right,
    discrete_m = discrete_m,
    winsorize_low = winsorize_low,
    winsorize_high = winsorize_high,
    pd_n = pd_n,
    ...
  )
}

