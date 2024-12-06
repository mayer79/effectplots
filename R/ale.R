#' Accumulated Local Effects (ALE)
#'
#' @description
#' Calculates ALE for one or multiple continuous features specified by `X`.
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
#' ALE fixes the curse as follows: Per bin, the local effect is calculated as the
#' partial dependence difference between lower and upper bin break, using only
#' observations falling into this bin. This is repeated for all bins,
#' and the values are *accumulated*.
#'
#' ALE values are plotted against right bin breaks.
#'
#' @details
#' The function is a convenience wrapper around [feature_effects()], which calls
#' the barebone implementation [.ale()] to calculate ALE.
#'
#' @inheritParams feature_effects
#' @param discrete_m Numeric features with up to this number of unique values are
#'   treated as discrete and are therefore dropped from the calculations.
#' @param seed Optional integer random seed used for:
#'   - *ALE:* select background data if `n > ale_n`, and for bins > `ale_bin_size`.
#'   - *Calculating breaks:* The bin range is determined without values outside
#'     quartiles +- 2 IQR using a sample of <= 9997 observations to calculate quartiles.
#' @inherit feature_effects return
#' @references
#'   Apley, Daniel W., and Jingyu Zhu. 2020. *Visualizing the Effects of Predictor Variables in Black Box Supervised Learning Models.*
#'     Journal of the Royal Statistical Society Series B: Statistical Methodology,
#'     82 (4): 1059–1086. doi:10.1111/rssb.12377.
#' @seealso [feature_effects()], [.ale()]
#' @export
#' @examples
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' M <- ale(fit, v = "Petal.Length", data = iris)
#' M |> plot()
#'
#' M2 <- ale(fit, v = colnames(iris)[-1], data = iris, breaks = 5)
#' plot(M2, share_y = "all")  # Only continuous variables shown
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
    discrete_m = 13L,
    outlier_iqr = 2,
    ale_n = 50000L,
    ale_bin_size = 200L,
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
    pd_n = 0L,
    ale_n = ale_n,
    ale_bin_size = ale_bin_size,
    seed = seed,
    ...
  )
}

#' @describeIn ale Method for ranger models.
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
    discrete_m = 13L,
    outlier_iqr = 2,
    ale_n = 50000L,
    ale_bin_size = 200L,
    seed = NULL,
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
    ale_n = ale_n,
    ale_bin_size = ale_bin_size,
    seed = seed,
    ...
  )
}

#' @describeIn ale Method for DALEX explainers
#' @export
ale.explainer <- function(
    object,
    v = colnames(data),
    data = object$data,
    pred_fun = object$predict_function,
    trafo = NULL,
    which_pred = NULL,
    w = object$weights,
    breaks = "Sturges",
    right = TRUE,
    discrete_m = 13L,
    outlier_iqr = 2,
    ale_n = 50000L,
    ale_bin_size = 200L,
    seed = NULL,
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
    ale_n = ale_n,
    ale_bin_size = ale_bin_size,
    seed = seed,
    ...
  )
}

#' @describeIn ale Method for H2O models
#' @export
ale.H2OModel <- function(
    object,
    data,
    v = object@parameters$x,
    pred_fun = NULL,
    trafo = NULL,
    which_pred = NULL,
    w = object@parameters$weights_column$column_name,
    breaks = "Sturges",
    right = TRUE,
    discrete_m = 13L,
    outlier_iqr = 2,
    ale_n = 50000L,
    ale_bin_size = 200L,
    seed = NULL,
    ...
) {
  if (!requireNamespace("h2o", quietly = TRUE)) {
    stop("Package 'h2o' not installed")
  }
  stopifnot(is.data.frame(data) || inherits(data, "H2OFrame"))
  if (inherits(data, "H2OFrame")) {
    data <- as.data.frame(data)
  }
  if (is.null(pred_fun)) {
    pred_fun <- function(model, data, ...) {
      xvars <- model@parameters$x
      stats::predict(model, h2o::as.h2o(collapse::ss(data, , xvars)), ...)
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
    ale_n = ale_n,
    ale_bin_size = ale_bin_size,
    seed = seed,
    ...
  )
}

#' Barebone Accumulated Local Effects (ALE)
#'
#' This is a barebone implementation of Apley's ALE.
#' Per bin, the local effect \eqn{D_j} is calculated, and then accumulated over bins.
#' \eqn{D_j} equals the difference between the partial dependence at the
#' lower and upper bin breaks using only observations within bin.
#' To plot the values, we can make a line plot of the resulting vector against
#' upper bin breaks. Alternatively, the vector can be extended
#' from the left by the value 0, and then plotted against *all* breaks.
#'
#' @param v Variable name in `data` to calculate ALE.
#' @param data Matrix or data.frame.
#' @param breaks Bin breaks.
#' @param right Should bins be right-closed?
#'   The default is `TRUE`. (No effect if `g` is provided.)
#' @param bin_size Maximal number of observations used per bin. If there are more
#'   observations in a bin, `bin_size` indices are randomly sampled. The default is 200.
#' @param w Optional vector with case weights.
#' @param g For internal use. The result of `as.factor(findInterval(...))`.
#'   By default `NULL`.
#' @inheritParams feature_effects
#' @returns Vector representing one ALE per bin.
#' @export
#' @seealso [partial_dependence()]
#' @inherit ale references
#' @examples
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' v <- "Sepal.Width"
#' .ale(fit, v, data = iris, breaks = seq(2, 4, length.out = 5))
.ale <- function(
    object,
    v,
    data,
    breaks,
    right = TRUE,
    pred_fun = stats::predict,
    trafo = NULL,
    which_pred = NULL,
    bin_size = 200L,
    w = NULL,
    g = NULL,
    ...
) {
  if (is.null(g)) {
    x <- if (is.data.frame(data)) data[[v]] else data[, v]
    g <- findInterval(
      x, vec = breaks, rightmost.closed = TRUE, left.open = right, all.inside = TRUE
    )
    g <- collapse::qF(g, sort = FALSE)
  }

  # List of bin indices. We remove empty or NA bins.
  J <- lapply(
    collapse::gsplit(g = g, use.g.names = TRUE),
    function(z) if (length(z) <= bin_size) z else sample(z, size = bin_size)
  )
  ok <- !is.na(names(J)) & lengths(J, use.names = FALSE) > 0L
  if (!all(ok)) {
    J <- J[ok]
  }

  # Before flattening the list J, we store bin counts
  bin_n <- lengths(J, use.names = FALSE)
  ix <- as.integer(names(J))
  J <- unlist(J, recursive = FALSE, use.names = FALSE)

  # Empty bins will get an incremental effect of 0
  out <- numeric(length(breaks) - 1L)

  # Now we create a single prediction dataset. Lower bin edges first, then upper ones.
  data_long <- collapse::ss(data, rep.int(J, 2L))
  grid_long <- rep.int(c(breaks[ix], breaks[ix + 1L]), times = c(bin_n, bin_n))
  if (is.data.frame(data_long)) {
    data_long[[v]] <- grid_long
  } else {
    data_long[, v] <- grid_long
  }
  pred <- prep_pred(
    pred_fun(object, data_long, ...), trafo = trafo, which_pred = which_pred
  )
  n <- length(J)
  out[ix] <- collapse::fmean(
    pred[(n + 1L):(2L * n)] - pred[1L:n],
    g = collapse::fdroplevels(g[J]),
    w = if (!is.null(w)) w[J],
    use.g.names = FALSE
  )
  return(cumsum(out))
}
