#' Feature Effects
#'
#' @description
#' This is the main function of the package. By default, it calculates
#' the following statistics per feature X over values/bins:
#' - "y_mean": Average observed `y` values. Used to assess descriptive associations
#'   between response and features.
#' - "pred_mean": Average predictions. Corresponds to "M Plots" (from  "marginal")
#'   in Apley (2020). Shows the combined effect of X and other (correlated) features.
#'   The difference to average observed y values shows model bias.
#' - "resid_mean": Average residuals. Calculated when
#'   both `y` and predictions are available. Useful to study model bias.
#' - "pd": Partial dependence (Friedman, 2001): See [partial_dependence()].
#'   Evaluated at bin averages, not at bin midpoints.
#' - "ale": Accumulated local effects (Apley, 2020): See [ale()]. Only for numeric X.
#'
#' Additionally, corresponding counts/weights are calculated, and
#' standard deviations of observed y, predictions, and residuals.
#'
#' Numeric X with more than `discrete_m = 5` disjoint values are binned as in
#' [graphics::hist()] via `breaks`. Before calculating bins, outliers are capped
#' at +-2 IQR from the quartiles.
#'
#' All averages and standard deviation are weighted by optional weights `w`.
#'
#' If you need only one specific statistic, you can use the simplified APIs of
#' - [average_observed()],
#' - [average_predicted()],
#' - [bias()],
#' - [partial_dependence()], and
#' - [ale()].
#'
#' @param object Fitted model.
#' @param v Vector of variable names to calculate statistics.
#' @param data Matrix or data.frame.
#' @param y Numeric vector with observed values of the response.
#'   Can also be a column name in `data`. Omitted if `NULL` (default).
#' @param pred Numeric vector with predictions. If `NULL`, it is calculated as
#'   `pred_fun(object, data, ...)`. Used to save time if `d()` is to be
#'   called multiple times.
#' @param pred_fun Prediction function, by default `stats::predict`.
#'   The function takes three arguments (names irrelevant): `object`, `data`, and `...`.
#' @param trafo How should predictions be transformed?
#'   A function or `NULL` (default). Examples are `log` (to switch to link scale)
#'   or `exp` (to switch from link scale to the original scale).
#' @param which_pred If the predictions are multivariate: which column to pick
#'   (integer or column name). By default `NULL` (picks last column).
#' @param w Optional vector with case weights. Can also be a column name in `data`.
#' @param breaks An integer, vector, string or function specifying the bins
#'   of the numeric X variables as in [graphics::hist()]. The default is "Sturges".
#'   To allow varying values of `breaks` across variables, it can be a list of the
#'   same length as `v`, or a *named* list with `breaks` for certain variables.
#' @param right Should bins be right-closed? The default is `TRUE`.
#'   Vectorized over `v`. Only relevant for numeric X.
#' @param discrete_m Numeric X variables with up to this number of unique values
#'   should not be binned and treated as a factor (after calculating partial dependence)
#'   The default is 5. Vectorized over `v`.
#' @param outlier_iqr Outliers of a numeric X are capped via the boxplot rule, i.e.,
#'   outside `outlier_iqr` * IQR from the quartiles. The default is 2 is more
#'   conservative than the usual rule to account for right-skewed distributions.
#'   Set to 0 or `Inf` for no capping. Note that at most 10k observations are sampled
#'   to calculate quartiles (uses random seed). Vectorized over `v`.
#' @param calc_pred Should predictions be calculated? Default is `TRUE`. Only relevant
#'   if `pred = NULL`.
#' @param pd_n Size of the data used for calculating partial dependence.
#'   The default is 500. For larger `data` (and `w`), `pd_n` rows are randomly sampled
#'   (uses random seed). Each variable specified by `v` uses the same subsample.
#'   Set to 0 to omit.
#' @param ale_n Size of the data used for calculating ALE.
#'   The default is 50000. For larger `data` (and `w`), `ale_n` rows are randomly
#'   sampled (uses random seed). Each variable specified by `v` uses the same subsample.
#'   Set to 0 to omit.
#' @param ale_bin_size Maximal number of observations used per bin for ALE calculations.
#'   If there are more observations in a bin, `ale_bin_size` indices are
#'   randomly sampled (uses random seed). The default is 200. Applied after subsampling
#'   regarding `ale_n`.
#' @param ... Further arguments passed to `pred_fun()`, e.g., `type = "response"` in
#'   a `glm()` or (typically) `prob = TRUE` in classification models.
#' @returns
#'   A list (of class "EffectData") with a data.frame of statistics per feature. Use
#'   single bracket subsetting to select part of the output.
#' @seealso [plot.EffectData()], [update.EffectData()], [partial_dependence()],
#'   [ale()], [average_observed], [average_predicted()], [bias()]
#' @references
#'   1. Molnar, Christoph. 2019. *Interpretable Machine Learning: A Guide for Making Black Box Models Explainable*.
#'     <https://christophm.github.io/interpretable-ml-book>.
#'   2. Friedman, Jerome H. 2001, *Greedy Function Approximation: A Gradient Boosting Machine.*
#'     Annals of Statistics 29 (5): 1189-1232. doi:10.1214/aos/1013203451.3.
#'   3. Apley, Daniel W., and Jingyu Zhu. 2016. *Visualizing the Effects of Predictor Variables in Black Box Supervised Learning Models.*
#'     Journal of the Royal Statistical Society Series B: Statistical Methodology,
#'     82 (4): 1059–1086. doi:10.1111/rssb.12377.
#' @export
#' @examples
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' xvars <- colnames(iris)[-1]
#' M <- feature_effects(fit, v = xvars, data = iris, y = "Sepal.Length", breaks = 5)
#' M
#' M |> update(sort = "pd") |> plot(share_y = "all")
feature_effects <- function(object, ...) {
  UseMethod("feature_effects")
}

#' @describeIn feature_effects Default method.
#' @export
feature_effects.default <- function(
    object,
    v,
    data,
    y = NULL,
    pred = NULL,
    pred_fun = stats::predict,
    trafo = NULL,
    which_pred = NULL,
    w = NULL,
    breaks = "Sturges",
    right = TRUE,
    discrete_m = 5L,
    outlier_iqr = 2,
    calc_pred = TRUE,
    pd_n = 500L,
    ale_n = 50000L,
    ale_bin_size = 200L,
    ...
) {
  # Input checks
  stopifnot(
    is.data.frame(data) || is.matrix(data),
    v %in% colnames(data),
    is.function(pred_fun),
    is.null(trafo) || is.function(trafo),
    pd_n >= 0L,
    ale_n >= 0L,
    ale_bin_size >= 0L
  )
  n <- nrow(data)
  nms <- colnames(data)
  stopifnot(
    n >= 2L,
    basic_check(y, n = n, nms = nms),
    basic_check(pred, n = n, nms = nms),
    basic_check(w, n = n, nms = nms)
  )

  # Prepare y
  if (!is.null(y)) {
    if (length(y) == 1L) {
      y <- if (is.matrix(data)) data[, y] else data[[y]]
    }
    if (!is.numeric(y) && !is.logical(y)) {
      stop("'y' must be numeric or logical.")
    }
    if (anyNA(y)) {
      stop("'y' can't contain NA")
    }
    if (!is.double(y)) {
      y <- as.double(y)
    }
  }

  # Prepare pred (part 1)
  if (!is.null(pred)) {
    pred <- prep_pred(pred, trafo = trafo, which_pred = which_pred)
  }

  # Prepare w
  if (!is.null(w)) {
    if (length(w) == 1L) {
      w <- if (is.matrix(data)) data[, w] else data[[w]]
    }
    if (!is.numeric(w) && !is.logical(w)) {
      stop("'w' must be numeric, or logical.")
    }
    wpos <- !is.na(w) & w > 0
    if (!any(wpos)) {
      stop("No positive 'w'!")
    }
    if (!all(wpos)) {
      message("Removing data with missing or non-positive weights 'w'.")
      w <- w[wpos]
      data <- collapse::ss(data, wpos)
      n <- nrow(data)
      stopifnot(n >= 2L)
      if (!is.null(y)) {
        y <- y[wpos]
      }
      if (!is.null(pred)) {
        pred <- pred[wpos]
      }
    }
    if (!is.double(w)) {
      w <- as.double(w)
    }
  }

  # Prepare pred (part 2)
  if (is.null(pred) && isTRUE(calc_pred)) {
    pred <- prep_pred(
      pred_fun(object, data, ...), trafo = trafo, which_pred = which_pred
    )
  }

  re <- !is.null(pred) && !is.null(y)
  PYR <- cbind(pred = pred, y = y, resid = if (re) y - pred) # cbind(NULL, NULL) is NULL

  # Prepare pd_data and ale_data (list with data, w, ix)
  pd_data <- if (pd_n > 0L) .subsample(data, nmax = pd_n, w = w)
  ale_data <- if (ale_n > 0L) .subsample(data, nmax = ale_n, w = w)

  # Prepare breaks
  nv <- length(v)
  if (is.list(breaks)) {
    if (length(breaks) < nv) {
      br <- replicate(nv, "Sturges", simplify = FALSE)
      names(br) <- v
      br[names(breaks)] <- breaks
      breaks <- br
    }
  } else {
    breaks <- replicate(nv, breaks, simplify = FALSE)
  }

  out <- mapply(
    FUN = calculate_stats,
    v,
    breaks = breaks,
    right = right,
    discrete_m = discrete_m,
    outlier_iqr = outlier_iqr,
    MoreArgs = list(
      PYR = PYR,
      w = w,
      data = data,
      object = object,
      pred_fun = pred_fun,
      trafo = trafo,
      which_pred = which_pred,
      pd_data = pd_data,
      ale_data = ale_data,
      ale_bin_size,
      ...
    ),
    SIMPLIFY = FALSE
  )

  # Remove empty results (happens if feature is discrete and only ALE was calculated)
  ok <- lengths(out) > 0L  # non-null (has some columns)
  if (!all(ok)) {
    if (!any(ok)) {
      stop("Nothing has been calculated!")
    }
    message(
      "Dropping variables without results: ", paste(names(out)[!ok], collapse = ", ")
    )
    out <- out[ok]
  }
  structure(out, class = "EffectData")
}

#' @describeIn feature_effects Method for "ranger" models.
#' @export
feature_effects.ranger <- function(
    object,
    v,
    data,
    y = NULL,
    pred = NULL,
    pred_fun = NULL,
    trafo = NULL,
    which_pred = NULL,
    w = NULL,
    breaks = "Sturges",
    right = TRUE,
    discrete_m = 5L,
    outlier_iqr = 2,
    calc_pred = TRUE,
    pd_n = 500L,
    ale_n = 50000L,
    ale_bin_size = 200L,
    ...
) {
  if (is.null(pred_fun)) {
    pred_fun <- function(model, newdata, ...) {
      stats::predict(model, newdata, ...)$predictions
    }
  }
  feature_effects.default(
    object,
    v = v,
    data = data,
    y = y,
    pred = pred,
    pred_fun = pred_fun,
    trafo = trafo,
    which_pred = which_pred,
    w = w,
    breaks = breaks,
    right = right,
    discrete_m = discrete_m,
    outlier_iqr = outlier_iqr,
    calc_pred = calc_pred,
    pd_n = pd_n,
    ale_n = ale_n,
    ale_bin_size = ale_bin_size,
    ...
  )
}

#' @describeIn feature_effects Method for DALEX "explainer".
#' @export
feature_effects.explainer <- function(
  object,
  v,
  data = object[["data"]],
  y = NULL,
  pred = NULL,
  pred_fun = object[["predict_function"]],
  trafo = NULL,
  which_pred = NULL,
  w = object[["weights"]],
  breaks = "Sturges",
  right = TRUE,
  discrete_m = 5L,
  outlier_iqr = 2,
  calc_pred = TRUE,
  pd_n = 500L,
  ale_n = 50000L,
  ale_bin_size = 200L,
  ...
) {
  feature_effects.default(
    object,
    v = v,
    data = data,
    y = y,
    pred = pred,
    pred_fun = pred_fun,
    trafo = trafo,
    which_pred = which_pred,
    w = w,
    breaks = breaks,
    right = right,
    discrete_m = discrete_m,
    outlier_iqr = outlier_iqr,
    calc_pred = calc_pred,
    pd_n = pd_n,
    ale_n = ale_n,
    ale_bin_size = ale_bin_size,
    ...
  )
}

