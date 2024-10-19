#' Partial Dependence
#'
#' Calculates partial dependence of one or more variables (`v`) in
#' a dataset `data`. This function is a convenience wrapper over [marginal()].
#'
#' @inheritParams marginal
#' @inherit marginal return
#' @references
#'   Friedman, Jerome H. *"Greedy Function Approximation: A Gradient Boosting Machine."*
#'     Annals of Statistics 29, no. 5 (2001): 1189-1232.
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
    w = NULL,
    breaks = "Sturges",
    right = TRUE,
    discrete_m = 2L,
    wprob_low = 0.01,
    wprob_high = 0.99,
    pd_n = 500L,
    ...
) {
  marginal.default(
    object = object,
    v = v,
    data = data,
    y = NULL,
    pred_fun = pred_fun,
    w = w,
    breaks = breaks,
    right = right,
    discrete_m = discrete_m,
    wprob_low = wprob_low,
    wprob_high = wprob_high,
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
    w = NULL,
    breaks = "Sturges",
    right = TRUE,
    discrete_m = 2L,
    wprob_low = 0.01,
    wprob_high = 0.99,
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
    w = w,
    breaks = breaks,
    right = right,
    discrete_m = discrete_m,
    wprob_low = wprob_low,
    wprob_high = wprob_high,
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
    w = object[["weights"]],
    breaks = "Sturges",
    right = TRUE,
    discrete_m = 2L,
    wprob_low = 0.01,
    wprob_high = 0.99,
    pd_n = 500L,
    ...
) {
  partial_dependence.default(
    object = object[["model"]],
    v = v,
    data = data,
    pred_fun = pred_fun,
    w = w,
    breaks = breaks,
    right = right,
    discrete_m = discrete_m,
    wprob_low = wprob_low,
    wprob_high = wprob_high,
    pd_n = pd_n,
    ...
  )
}

