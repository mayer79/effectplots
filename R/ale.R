#' Accumulated Local Effects (ALE)
#'
#' @description
#' Calculates ALE for one or multiple `X` variables.
#'
#' The concept of ALE was introduced in Apley et al. (2020) as an alternative to
#' partial dependence (PD). The Ceteris Paribus clause behind PD is a blessing and
#' a curse at the same time:
#'
#' - Blessing: The interpretation is easy and similar to what we know from linear
#'   regression  (just averaging out interaction effects).
#' - Curse: The model is applied to very unlikely or even impossible feature
#'   combinations, especially with strongly dependent features.
#'
#' ALE fixes the curse as follows: Partial dependence is calculated for the lower and
#' upper endpoint of a bin, using all (or a sample) of observations falling into this
#' bin. Its slope provides the *local effect* over the bin.
#' This is repeated for all bins, and the values are *accumulated*. Since the resulting
#' sum starts at 0, one typically shifts the result vertically, e.g., to the average
#' prediction. This is not done by [ale()], however.
#'
#' The function is a convenience wrapper around [marginal()], which calls
#' the barebone implementation [.ale()] to calculate ALE. The ALE values calculated
#' by [marginal()] are vertically shifted to the same (weighted) average than the
#' partial dependence curve, for optimal comparability.
#'
#' @inheritParams marginal
#' @inherit marginal return
#' @references
#'   Apley, Daniel W., and Jingyu Zhu. 2020. *Visualizing the Effects of Predictor Variables in Black Box Supervised Learning Models.*
#'     Journal of the Royal Statistical Society Series B: Statistical Methodology,
#'     82 (4): 1059–1086. doi:10.1111/rssb.12377.
#' @seealso [marginal()], [.ale()]
#' @export
#' @examples
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' M <- ale(fit, v = "Petal.Length", data = iris)
#' M |> plot()
#'
#' M2 <- ale(fit, v = colnames(iris)[-1], data = iris, breaks = 5)
#' plot(M2, share_y = TRUE)  # Only numeric variables shown
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
