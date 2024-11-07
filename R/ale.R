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
#' The function is a convenience wrapper around [feature_effects()], which calls
#' the barebone implementation [.ale()] to calculate ALE. The ALE values calculated
#' by [feature_effects()] are vertically shifted to the same (weighted) average than the
#' partial dependence curve, for optimal comparability.
#'
#' @inheritParams feature_effects
#' @param seed Optional random seed (an integer) used for:
#'   - ALE: select background data if `n > ale_n` and for bins > `ale_bin_size`.
#'   - Capping X: quartiles are selected based on 10k observations.
#'   Note that the current `.Random.seed` is restored on function exit, i.e.,
#'   setting the seed does not affect the rest of your R session.
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
#' plot(M2, share_y = "all")  # Only numeric variables shown
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

#' Barebone Accumulated Local Effects (ALE)
#'
#' This is a barebone implementation of Apley's ALE intended for developers.
#' To get more information on ALE, see [ale()].
#'
#' @param v Variable name in `data` to calculate ALE.
#' @param data Matrix or data.frame.
#' @param breaks Breaks for ALE calculation.
#' @param right Should bins specified via `breaks` be right-closed?
#'   The default is `TRUE`.
#' @param bin_size Maximal number of observations used per bin. If there are more
#'   observations in a bin, `bin_size` indices are randomly sampled. The default is 200.
#' @param w Optional vector with case weights.
#' @param g For internal use. The result of `qF(findInterval(...))`.
#'   By default `NULL`.
#' @inheritParams feature_effects
#' @returns Vector of ALE values in the same order as `breaks[-length(breaks)]`.
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

  # List of bin indices. Eventual NA levels are placed at the end. We will remove it.
  J <- lapply(
    collapse::gsplit(1:length(g), g = g, use.g.names = TRUE),
    function(z) if (length(z) <= bin_size) z else sample(z, size = bin_size)
  )
  if (anyNA(names(J))) {
    J <- J[!is.na(names(J))]
  }

  # Before flattening the list J, we store bin counts
  bin_n <- lengths(J, use.names = FALSE)
  ix <- as.integer(names(J))
  J <- unlist(J, recursive = FALSE, use.names = FALSE)

  # Empty bins will get an incremental effect of 0
  p <- length(breaks) - 1L
  out <- numeric(p)

  # Now we create a single prediction dataset. Lower bin edges first, then upper ones.
  data_long <- collapse::ss(data, rep.int(J, 2L))
  grid_long <- rep.int(
    c(breaks[-(p + 1L)][ix], breaks[-1L][ix]), times = c(bin_n, bin_n)
  )
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
    pred[(n + 1L):(2L * n)] - pred[1L:n], g = g[J], w = if (!is.null(w)) w[J]
  )
  return(cumsum(out))
}
