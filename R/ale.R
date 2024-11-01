#' Accumulated Local Effects (ALE)
#'
#' Calculates uncentered ALE of one or more X variables (`v`) in `data`.
#' This function is a convenience wrapper around [marginal()].
#'
#' @inheritParams marginal
#' @inherit marginal return
#' @references
#'   Apley, Daniel W., and Jingyu Zhu. 2016. *Visualizing the Effects of Predictor Variables in Black Box Supervised Learning Models.*
#'     Journal of the Royal Statistical Society Series B: Statistical Methodology,
#'     82 (4): 1059–1086. doi:10.1111/rssb.12377.
#' @seealso [marginal()]
#' @export
#' @examples
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' M <- ale(fit, v = "Petal.Length", data = iris)
#' M |> plot()
#'
#' M2 <- ale(fit, v = colnames(iris)[-1], data = iris)
#' plot(M2, share_y = TRUE)
ale <- function(object, ...) {
  UseMethod("ale")
}

#' @describeIn ale Default method.
#' @export
ale.default <- function(
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
    ale_bin_size = 200L,
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
    pd_n = 0L,
    ale_bin_size = ale_bin_size,
    ...
  )
}

#' @describeIn ale Default method.
#' @export
ale.ranger <- function(
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
    ale_bin_size = 200L,
    ...
) {
  if (is.null(pred_fun)) {
    pred_fun <- function(model, newdata, ...) {
      stats::predict(model, newdata, ...)$predictions
    }
  }
  ale.default(
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
    ale_bin_size = ale_bin_size,
    ...
  )
}

#' @describeIn ale Default method.
#' @export
ale.explainer <- function(
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
    ale_bin_size = 200L,
    ...
) {
  ale.default(
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
    ale_bin_size = ale_bin_size,
    ...
  )
}
