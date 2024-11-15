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
#' This function is a convenience wrapper around [feature_effects()], which calls
#' the barebone implementation [.pd()] to calculate PD.
#' As grid points, it uses the arithmetic mean of `X` per bin (specified by `breaks`),
#' and eventually weighted by `w`.
#'
#' @inheritParams feature_effects
#' @param seed Optional random seed (an integer) used for:
#'   - Partial dependence: select background data if `n > pd_n`.
#'   - Capping X: quartiles are selected based on 10k observations.
#' @inherit feature_effects return
#' @references
#'   Friedman, Jerome H. 2001, *Greedy Function Approximation: A Gradient Boosting Machine.*
#'     Annals of Statistics 29 (5): 1189-1232. doi:10.1214/aos/1013203451.
#' @seealso [feature_effects()], [.pd()], [ale()].
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
    seed = NULL,
    ...
) {
  feature_effects.default(
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
    ale_n = 0L,
    seed = seed,
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
    seed = NULL,
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
    seed = seed,
    ...
  )
}

#' @describeIn partial_dependence Default method.
#' @export
partial_dependence.explainer <- function(
    object,
    v = colnames(data),
    data = object$data,
    pred_fun = object$predict_function,
    trafo = NULL,
    which_pred = NULL,
    w = object$weights,
    breaks = "Sturges",
    right = TRUE,
    discrete_m = 5L,
    outlier_iqr = 2,
    pd_n = 500L,
    seed = NULL,
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
    seed = seed,
    ...
  )
}

#' Barebone Partial Dependence
#'
#' This is a barebone implementation of Friedman's partial dependence
#' intended for developers. To get more information on partial dependence, see
#' [partial_dependence()].
#'
#' @param v Variable name in `data` to calculate partial dependence.
#' @param data Matrix or data.frame.
#' @param grid Vector or factor of values to calculate partial dependence for.
#' @param w Optional vector with case weights.
#' @inheritParams feature_effects
#' @returns Vector of partial dependence values in the same order as `grid`.
#' @export
#' @seealso [partial_dependence()]
#' @inherit partial_dependence references
#' @examples
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' .pd(fit, "Sepal.Width", data = iris, grid = hist(iris$Sepal.Width)$mids)
#' .pd(fit, "Species", data = iris, grid = levels(iris$Species))
.pd <- function(
    object,
    v,
    data,
    grid,
    pred_fun = stats::predict,
    trafo = NULL,
    which_pred = NULL,
    w = NULL,
    ...
) {
  n <- nrow(data)
  p <- length(grid)
  data_long <- collapse::ss(data, rep.int(seq_len(n), p))
  grid_long <- rep(grid, each = n)
  if (is.data.frame(data_long)) {
    data_long[[v]] <- grid_long
  } else {
    data_long[, v] <- grid_long
  }
  pred <- prep_pred(
    pred_fun(object, data_long, ...), trafo = trafo, which_pred = which_pred
  )
  dim(pred) <- c(n, p)
  collapse::fmean.matrix(pred, w = w, use.g.names = FALSE)
}
