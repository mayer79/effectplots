#' Marginal Statistics
#'
#' This is the main function of the package. It calculates
#' - average observed,
#' - average predicted,
#' - partial dependence, and
#' - counts/weights
#' over a (possibly binned) feature v, optionally weighted with weights `w`.
#'
#' @param object Fitted model.
#' @param v Names of the variables shown on the x axis.
#' @param data Matrix-like.
#' @param y Numeric vector with observed values of the response.
#'   Can also be a column name in `data`. Omitted if `NULL` (default).
#' @param pred Numeric vector with predictions. If `NULL`, it is calculated as
#'   `pred_fun(object, data, ...)`. Used to save time if `marginal()` is to be
#'   called multiple times.
#' @param pred_fun Prediction function, by default `stats::predict`.
#'   The function takes three arguments (names irrelevant): `object`, `data`, and `...`.
#' @param w Optional vector with case weights. Can also be a column name in `data`.
#' @param breaks An integer, a vector, a string or a function specifying the bins
#'   of `x`, and passed to [graphics::hist()]. The default is "Sturges".
#'   *Not* vectorized over `v`. Only relevant for numeric x.
#' @param right Should bins created via [graphics::hist()] be right-closed?
#'   The default is `TRUE`. Vectorized over `v`. Only relevant for numeric x.
#' @param discrete_m Numeric x with up to this number of unique values
#'   should be treated as factors. The default is 2. Vectorized over `v`.
#' @param winsorize_low Small values of numeric x are capped at this quantile.
#'   Set to 0 to avoid Winsorizing. Note that at most 100k observations are sampled
#'   to calculate the quantile (depends on your random seed). Vectorized over `v`.
#' @param winsorize_high High values of numeric x are capped at this quantile.
#'   Set to 1 to avoid Winsorizing. Note that at most 100k observations are sampled
#'   to calculate the quantile (depends on your random seed). Vectorized over `v`.
#' @param calc_pred Should predictions be calculated? Default is `TRUE`. Only relevant
#'   if `pred = NULL`.
#' @param pd_n Size of the data used for calculation of partial dependence.
#'   The default is 500. Set to 0 (or pass `pred_fun = NULL`) to omit calculation
#'   of partial dependence. This depends on your random seed.
#' @param ... Further arguments passed to `pred_fun()`, e.g., `type = "response"` in
#'   a `glm()` or (typically) `prob = TRUE` in binary probabilistic models.
#' @returns
#'   If `v` has length 1, an object of class "marginal" containing these elements:
#'   - `data`: data.frame containing statistics and plot positions of values and bars.
#'   - `num`: Indicator whether x is numeric.
#'   - `v`: Same as input `v`.
#'   If `v` has length > 1, an object of class "multimarginal", which is a named
#'   list of "marginal" objects.
#' @seealso [average_observed()], [partial_dependence()]
#' @export
#' @examples
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' M <- marginal(
#'   fit, v = colnames(iris)[-1], data = iris, y = "Sepal.Length", breaks = 5
#' )
#' M$Petal.Width
#'
#' # Use plot option 'backend = "plotly"' for interactive plots
#' M |> plot()
marginal <- function(object, ...) {
  UseMethod("marginal")
}

#' @describeIn marginal Default method.
#' @export
marginal.default <- function(
    object,
    v,
    data,
    y = NULL,
    pred = NULL,
    pred_fun = stats::predict,
    w = NULL,
    breaks = "Sturges",
    right = TRUE,
    discrete_m = 2L,
    winsorize_low = 0.01,
    winsorize_high = 0.99,
    calc_pred = TRUE,
    pd_n = 500L,
    ...
) {
  stopifnot(
    is.data.frame(data) || is.matrix(data),
    is.function(pred_fun),
    v %in% colnames(data),
    winsorize_low <= winsorize_high
  )

  # Prepare pred
  if (is.null(pred)) {
    if (isTRUE(calc_pred)) {
      pred <- prep_vector(pred_fun(object, data, ...))
    }
  } else {
    if (length(pred) != nrow(data)) {
      stop("'pred' should be a vector of length nrow(data), or NULL.")
    }
    pred <- prep_vector(pred)
  }

  # Prepare y
  if (!is.null(y)) {
    y <- prep_vector(name_or_vector(y, data))
  }

  # Prepare w
  if (!is.null(w)) {
    w <- prep_vector(name_or_vector(w, data))
    if (any(w < 0) || anyNA(w)) {
      stop("'w' can't have negative or missing values")
    }
  }

  # Deal with multiple v
  if (length(v) > 1L) {
    out <- mapply(
      v,
      FUN = marginal,
      right = right,
      discrete_m = discrete_m,
      winsorize_low = winsorize_low,
      winsorize_high = winsorize_high,
      MoreArgs = list(
        object = object,
        data = data,
        y = y,
        pred = pred,
        pred_fun = pred_fun,
        w = w,
        breaks = breaks,
        calc_pred = calc_pred,
        pd_n = pd_n,
        ...
      ),
      SIMPLIFY = FALSE
    )
    names(out) <- v
    class(out) <- "multimarginal"
    return(out)
  }

  # Prepare x
  x <- if (is.matrix(data)) data[, v] else data[[v]]
  if (is.numeric(x) && (winsorize_low > 0 || winsorize_high < 1)) {
    x <- winsorize(x, probs = c(winsorize_low, winsorize_high), nmax = 1e5)
  }

  out <- calculate_stats(
    x = x,
    pred = pred,
    y = y,
    w = w,
    breaks = breaks,
    right = right,
    discrete_m = discrete_m
  )

  if (pd_n >= 1L) {
    out$data$pd <- partial_dep(
      object = object,
      v = v,
      X = data,
      grid = out$data$eval_at,
      pred_fun = pred_fun,
      pd_n = pd_n,
      w = w,
      ...
    )
  }

  if (!out$num && !is.factor(out$data$bar_at)) {
    # To ensure nice plot scales
    out$data$bar_at <- out$data$eval_at <- factor(out$data$bar_at)
  }
  out$v <- v
  class(out) <- "marginal"
  out
}

#' @describeIn marginal Method for "ranger" models.
#' @export
marginal.ranger <- function(
    object,
    v,
    data,
    y = NULL,
    pred = NULL,
    pred_fun = NULL,
    w = NULL,
    breaks = "Sturges",
    right = TRUE,
    discrete_m = 2L,
    winsorize_low = 0.01,
    winsorize_high = 0.99,
    calc_pred = TRUE,
    pd_n = 500L,
    ...
) {
  if (is.null(pred_fun)) {
    pred_fun <- function(model, newdata, ...) {
      stats::predict(model, newdata, ...)$predictions
    }
  }
  marginal.default(
    object,
    v = v,
    data = data,
    y = y,
    pred = pred,
    pred_fun = pred_fun,
    w = w,
    breaks = breaks,
    right = right,
    discrete_m = discrete_m,
    winsorize_low = winsorize_low,
    winsorize_high = winsorize_high,
    calc_pred = calc_pred,
    pd_n = pd_n,
    ...
  )
}

#' @describeIn marginal Method for DALEX "explainer".
#' @export
marginal.explainer <- function(
  object,
  v,
  data = object[["data"]],
  y = NULL,
  pred = NULL,
  pred_fun = object[["predict_function"]],
  w = object[["weights"]],
  breaks = "Sturges",
  right = TRUE,
  discrete_m = 2L,
  winsorize_low = 0.01,
  winsorize_high = 0.99,
  calc_pred = TRUE,
  pd_n = 500L,
  ...
) {
  marginal.default(
    object,
    v = v,
    data = data,
    y = y,
    pred = pred,
    pred_fun = pred_fun,
    w = w,
    breaks = breaks,
    right = right,
    discrete_m = discrete_m,
    winsorize_low = winsorize_low,
    winsorize_high = winsorize_high,
    calc_pred = calc_pred,
    pd_n = pd_n,
    ...
  )
}

#' @export
print.marginal <- function(x, ...) {
  cat("'marginal' object: \n", sep = "")
  print(x$data)
  invisible(x)
}

#' @export
print.multimarginal <- function(x, ...) {
  cat(
    "The first element of this 'multimarginal' object of length ",
    length(x),
    ": \n",
    sep = ""
  )
  print(x[[1L]]$data)
  invisible(x)
}

