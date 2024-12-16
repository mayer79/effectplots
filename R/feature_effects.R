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
#' - "ale": Accumulated local effects (Apley, 2020): See [ale()].
#'   Only for continuous features.
#'
#' Additionally, corresponding counts/weights are calculated, and
#' standard deviations of observed y and residuals.
#'
#' Numeric features with more than `discrete_m = 13` disjoint values are binned via
#' `breaks`. If `breaks` is a single integer or "Sturges", the total bin range is
#' calculated without values outside +-2 IQR from the quartiles.
#' Values outside the bin range are placed in the outermost bins. Note that
#' at most 9997 observations are used to calculate quartiles and IQR.
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
#' @param v Variable names to calculate statistics for.
#' @param data Matrix or data.frame.
#' @param y Numeric vector with observed values of the response.
#'   Can also be a column name in `data`. Omitted if `NULL` (default).
#' @param pred Pre-computed predictions (as from `predict()/pred_fun()).
#'   If `NULL`, it is calculated as `pred_fun(object, data, ...)`.
#' @param pred_fun Prediction function, by default `stats::predict`.
#'   The function takes three arguments (names irrelevant): `object`, `data`, and `...`.
#' @param trafo How should predictions be transformed?
#'   A function or `NULL` (default). Examples are `log` (to switch to link scale)
#'   or `exp` (to switch from link scale to the original scale).
#'   Applied after `which_pred`.
#' @param which_pred If the predictions are multivariate: which column to pick
#'   (integer or column name). By default `NULL` (picks last column). Applied before
#'   `trafo`.
#' @param w Optional vector with case weights. Can also be a column name in `data`.
#'   Having observations with non-positive weight is equivalent to excluding them.
#' @param breaks An integer, vector, or "Sturges" (the default) used to determine
#'   bin breaks of continuous features. Values outside the total bin range are placed
#'   in the outmost bins. To allow varying values of `breaks` across features,
#'   `breaks` can be a list of the same length as `v`, or a *named* list with breaks
#'   for certain variables.
#' @param right Should bins be right-closed? The default is `TRUE`.
#'   Vectorized over `v`. Only relevant for continuous features.
#' @param discrete_m Numeric features with up to this number of unique values should not
#'   be binned but rather treated as discrete. The default is 13. Vectorized over `v`.
#' @param outlier_iqr If `breaks` is an integer or "Sturges", the breaks of a continuous
#'   feature are calculated without taking into account feature values outside
#'   quartiles +- `outlier_iqr` * IQR (where <= 9997 values are used to calculate the
#'   quartiles). To let the breaks cover the full data range, set `outlier_iqr` to
#'   0 or `Inf`. Vectorized over `v`.
#' @param calc_pred Should predictions be calculated? Default is `TRUE`. Only relevant
#'   if `pred = NULL`.
#' @param pd_n Size of the data used for calculating partial dependence.
#'   The default is 500. For larger `data` (and `w`), `pd_n` rows are randomly sampled.
#'   Each variable specified by `v` uses the same sample.
#'   Set to 0 to omit PD calculations.
#' @param ale_n Size of the data used for calculating ALE.
#'   The default is 50000. For larger `data` (and `w`), `ale_n` rows are randomly
#'   sampled. Each variable specified by `v` uses the same sample.
#'   Set to 0 to omit ALE calculations.
#' @param ale_bin_size Maximal number of observations used per bin for ALE calculations.
#'   If there are more observations in a bin, `ale_bin_size` indices are
#'   randomly sampled. The default is 200. Applied after sampling regarding `ale_n`.
#' @param seed Optional integer random seed used for:
#'   - *Partial dependence:* select background data if `n > pd_n`.
#'   - *ALE:* select background data if `n > ale_n`, and for bins > `ale_bin_size`.
#'   - *Calculating breaks:* The bin range is determined without values outside
#'     quartiles +- 2 IQR using a sample of <= 9997 observations to calculate quartiles.
#' @param ... Further arguments passed to `pred_fun()`, e.g., `type = "response"` in
#'   a `glm()` or (typically) `prob = TRUE` in classification models.
#' @returns
#'   A list (of class "EffectData") with a data.frame per feature having columns:
#'
#'   - `bin_mid`: Bin mid points. In the plots, the bars are centered around these.
#'   - `bin_width`: Absolute width of the bin. In the plots, these equal the bar widths.
#'   - `bin_mean`: For continuous features, the (possibly weighted) average feature
#'     value within bin. For discrete features equivalent to `bin_mid`.
#'   - `N`: The number of observations within bin.
#'   - `weight`: The weight sum within bin. When `w = NULL`, equivalent to `N`.
#'   - Different statistics, depending on the function call.
#'
#'   Use single bracket subsetting to select part of the output. Note that each
#'   data.frame contains an attribute "discrete" with the information whether the
#'   feature is discrete or continuous. This attribute might be lost when you manually
#'   modify the data.frames.
#' @seealso [plot.EffectData()], [update.EffectData()], [partial_dependence()],
#'   [ale()], [average_observed], [average_predicted()], [bias()]
#' @references
#'   1. Molnar, Christoph. 2019. *Interpretable Machine Learning: A Guide for Making Black Box Models Explainable*.
#'     <https://christophm.github.io/interpretable-ml-book/>.
#'   2. Friedman, Jerome H. 2001, *Greedy Function Approximation: A Gradient Boosting Machine.*
#'     Annals of Statistics 29 (5): 1189-1232. doi:10.1214/aos/1013203451.3.
#'   3. Apley, Daniel W., and Jingyu Zhu. 2016. *Visualizing the Effects of Predictor Variables in Black Box Supervised Learning Models.*
#'     Journal of the Royal Statistical Society Series B: Statistical Methodology,
#'     82 (4): 1059–1086. doi:10.1111/rssb.12377.
#' @export
#' @examples
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' xvars <- colnames(iris)[2:5]
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
    discrete_m = 13L,
    outlier_iqr = 2,
    calc_pred = TRUE,
    pd_n = 500L,
    ale_n = 50000L,
    ale_bin_size = 200L,
    seed = NULL,
    ...
) {
  # Input checks
  stopifnot(
    is.data.frame(data) || is.matrix(data),
    length(v) >= 1L,
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
    basic_check(w, n = n, nms = nms)
  )

  if (!is.null(seed)) {
    # old <- .Random.seed
    # on.exit({.Random.seed <- old})
    set.seed(seed)
  }

  # Prepare y
  if (!is.null(y)) {
    if (length(y) == 1L) {
      y <- if (is.matrix(data)) data[, y] else data[[y]]
    } else {
      y <- unname(y)
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
    if (NROW(pred) != n) {
      stop("'pred' must have the same length as nrow(data).")
    }
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
      stopifnot(nrow(data) >= 2L)
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

  # Check X variables
  if (is.matrix(data)) {
    stopifnot("Matrix is of wrong type" = check_v_type(data))
  } else {
    ok <- vapply(collapse::ss(data, , v), check_v_type, FUN.VALUE = logical(1L))
    if (!all(ok)) {
      stop("Unsupported data type for ", paste(v, collapse = ", "))
    }
  }

  # We need this subset for fast quartiles and fast check if numeric x is discrete
  ix_sub <- if (nrow(data) > 9997L) sample.int(nrow(data), 9997L)

  # Combine pred, y, and resid. If df, we can easier drop columns in grouped_stats()
  PYR <- list(pred = pred, y = y, resid = if (!is.null(pred) && !is.null(y)) y - pred)
  wPYR <- lengths(PYR) > 0L
  PYR <- if (any(wPYR)) collapse::qDF(PYR[wPYR])

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

  # We want to pass a list/data.frame to mapply(). For high length(v)/ncol(data),
  # the approach via qDF takes significantly less memory and time.
  if (is.matrix(data)) {
    if (nv <= ceiling(2 / 3 * ncol(data))) {
      data <- lapply(v, function(i) data[, i])
    } else {
      data <- collapse::qDF(data)
    }
  }

  out <- mapply(
    FUN = calculate_stats,
    v,
    x = if (is.data.frame(data)) collapse::ss(data, , v) else data,
    breaks = breaks,
    right = right,
    discrete_m = discrete_m,
    outlier_iqr = outlier_iqr,
    MoreArgs = list(
      PYR = PYR,
      w = w,
      object = object,
      pred_fun = pred_fun,
      trafo = trafo,
      which_pred = which_pred,
      pd_data = pd_data,
      ale_data = ale_data,
      ale_bin_size = ale_bin_size,
      ix_sub = ix_sub,
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

#' @describeIn feature_effects Method for ranger models.
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
    discrete_m = 13L,
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

#' @describeIn feature_effects Method for DALEX explainer.
#' @export
feature_effects.explainer <- function(
  object,
  v = colnames(data),
  data = object$data,
  y = object$y,
  pred = NULL,
  pred_fun = object$predict_function,
  trafo = NULL,
  which_pred = NULL,
  w = object$weights,
  breaks = "Sturges",
  right = TRUE,
  discrete_m = 13L,
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

#' @describeIn feature_effects Method for H2O models.
#' @export
feature_effects.H2OModel <- function(
    object,
    data,
    v = object@parameters$x,
    y = NULL,                   #  object@parameters$y does not work for multi-outputs
    pred = NULL,
    pred_fun = NULL,
    trafo = NULL,
    which_pred = NULL,
    w = object@parameters$weights_column$column_name,
    breaks = "Sturges",
    right = TRUE,
    discrete_m = 13L,
    outlier_iqr = 2,
    calc_pred = TRUE,
    pd_n = 500L,
    ale_n = 50000L,
    ale_bin_size = 200L,
    ...
) {
  if (!requireNamespace("h2o", quietly = TRUE)) {
    stop("Package 'h2o' not installed")
  }
  stopifnot(is.data.frame(data) || inherits(data, "H2OFrame"))
  if (inherits(data, "H2OFrame")) {
    if (is.null(pred) && calc_pred) {
      pred <- prep_pred(
        stats::predict(object, data, ...), trafo = trafo, which_pred = which_pred
      )
    }
    data <- as.data.frame(data)
  }

  if (is.null(pred_fun)) {
    pred_fun <- function(model, data, ...) {
      xvars <- model@parameters$x
      stats::predict(model, h2o::as.h2o(collapse::ss(data, , xvars)), ...)
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

#' Workhorse of feature_effects()
#'
#' Internal function used to calculate the output of `feature_effects()` for one
#' feature.
#'
#' @noRd
#' @keywords internal
#'
#' @param v One variable name.
#' @param x One feature vector/factor.
#' @param pd_data The output of .subsample() or `NULL`.
#' @param ale_data The output of .subsample() or `NULL`.
#' @param PYR A data.frame with predicted, observed, and residuals (if available). Can
#'   be `NULL` if none of them are available.
#' @param ix_sub Subset of 9997 indices, or `NULL` (if nrow(data) <= 9997).
#' @inheritParams feature_effects
#' @returns A data.frame with effect statistics.
calculate_stats <- function(
    v,
    x,
    PYR,
    w,
    breaks,
    right,
    discrete_m,
    outlier_iqr,
    object,
    pred_fun,
    trafo,
    which_pred,
    pd_data,
    ale_data,
    ale_bin_size,
    ix_sub,
    ...
) {
  # "factor", "double", "integer", "logical", "character"
  orig_type <- if (is.factor(x)) "factor" else typeof(x)
  was_ordered <- is.ordered(x)
  lev <- if (is.factor(x)) levels(x)

  x <- factor_or_double(x, m = discrete_m, ix_sub = ix_sub)
  discrete <- !is.numeric(x)

  if (is.null(PYR) && is.null(pd_data) && (discrete || is.null(ale_data))) {
    return(NULL)
  }

  sd_cols <- setdiff(colnames(PYR), "pred")  # Can be NULL

  # DISCRETE
  if (discrete) {
    M <- grouped_stats(PYR, g = x, w = w, sd_cols = sd_cols)

    # We need original unique values of g later for PDP, e.g., TRUE/FALSE.
    # Doubles might lose digits. This should not be a problem though.
    g <- parse_rownames(rownames(M), type = orig_type, ord = was_ordered, lev = lev)
    out <- data.frame(bin_mid = g, bin_width = 0.7, bin_mean = g, M)
    if (orig_type != "factor") {
      out <- out[order(g, na.last = TRUE), ]
    }
    if (orig_type %in% c("integer", "double") && length(stats::na.omit(g)) > 1L) {
      out$bin_width <- min(diff(out$bin_mid), na.rm = TRUE) * 0.7
    }
  } else {
    breaks <- fbreaks(x, breaks = breaks, outlier_iqr = outlier_iqr, ix_sub = ix_sub)
    mids <- 0.5 * (breaks[-1L] + breaks[-length(breaks)])
    bin_width <- diff(breaks)

    # Grouped stats
    ix <- fcut(x, breaks = breaks, right = right, explicit_na = TRUE)
    M <- grouped_stats(PYR, g = ix, w = w, sd_cols = sd_cols)

    if (anyNA(rownames(M))) {
      mids <- c(mids, NA)
      # We use half of the last bin width to fill the gap (NA value ~ discrete)
      na_bin_width <- bin_width[length(bin_width)] / 2
      bin_width <- c(bin_width, na_bin_width)
    }

    # Calculate bin_means, clip outliers, and replace missings (where possible)
    bin_means <- collapse::fmean(x, g = ix, w = w, use.g.names = FALSE)
    bad <- is.na(bin_means)
    bin_means[bad] = mids[bad]
    bin_means <- pmax(pmin(bin_means, breaks[length(breaks)]), breaks[1L])

    out <- data.frame(bin_mid = mids, bin_width = bin_width, bin_mean = bin_means, M)
  }
  rownames(out) <- NULL
  attr(out, "discrete") <- discrete

  # Turn numeric single-row NA into character for easier plotting
  if (nrow(out) == 1L && is.na(out$bin_mid) && is.numeric(out$bin_mid)) {
    out$bin_mid <- out$bin_mean <- NA_character_
  }

  # Add partial dependence
  if (!is.null(pd_data)) {
    out$pd <- .pd(
      object = object,
      v = v,
      data = pd_data$X,
      grid = out$bin_mean,
      pred_fun = pred_fun,
      trafo = trafo,
      which_pred = which_pred,
      w = pd_data$w,
      ...
    )
  }

  # Add ALE
  if (!is.null(ale_data)) {
    out$ale <- NA_real_
    if (!discrete) {
      ale <- .ale(
        object = object,
        v = v,
        data = ale_data$X,
        breaks = breaks,
        right = right,  # does not matter because we pass g
        pred_fun = pred_fun,
        trafo = trafo,
        which_pred = which_pred,
        bin_size = ale_bin_size,
        w = ale_data$w,
        g = if (is.null(ale_data$ix)) ix else ix[ale_data$ix],
        ...
      )
      ok <- !is.na(out$bin_mid)

      # Centering possible?
      cvars <- intersect(c("pd", "pred_mean", "y_mean"), colnames(out))
      if (length(cvars)) {
        w_ok <- out$weight[ok]
        ale_mids <- 0.5 * (ale + c(0, ale[-length(ale)]))  # average ALE per bin
        ale <- ale + collapse::fmean(out[[cvars[1L]]][ok], na.rm = TRUE, w = w_ok) -
          collapse::fmean(ale_mids, w = w_ok)
      }
      out$ale[ok] <- ale
    }
  }
  return(out)
}
