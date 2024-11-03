#' Partial Dependence
#'
#' @description
#'
#' Calculates PD for one or multiple `X` variables.
#'
#' PD was introduced by Friedman (2001) to study the (main) effects
#' of a ML model. PD of a model f and variable `X` at a certain value g
#' is derived by replacing the `X` values in a reference `data` by g,
#' and then calculating the average prediction of f over this modified data.
#' This is done for different g  to see how the average prediction of f changes in `X`,
#' keeping all other feature values constant (Ceteris Paribus).
#'
#' This function is a convenience wrapper around [marginal()], which calls
#' the barebone implementation [.pd()] to calculate PD.
#' As grid points, it uses the arithmetic mean of `X` per bin (specified by `breaks`),
#' and eventually weighted by `w`.
#'
#' @inheritParams marginal
#' @inherit marginal return
#' @references
#'   Friedman, Jerome H. 2001, *Greedy Function Approximation: A Gradient Boosting Machine.*
#'     Annals of Statistics 29 (5): 1189-1232. doi:10.1214/aos/1013203451.
#' @seealso [marginal()], [.pd()], [ale()].
#' @export
#' @examples
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' M <- partial_dependence(fit, v = "Species", data = iris)
#' M |> plot()
#'
#' M2 <- partial_dependence(fit, v = colnames(iris)[-1], data = iris)
#' plot(M2, share_y = "all")
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
    ale_bin_size = 0L,
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
