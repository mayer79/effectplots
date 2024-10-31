#' Marginal Statistics
#'
#' This is the main function of the package. It calculates
#' - average observed (with std),
#' - average predicted (with std),
#' - partial dependence,
#' - counts, and
#' - weights (same as counts if no weights `w` are passed)
#' over (possibly binned) features X specified by their column names `v`.
#'
#' For numeric variables with more than `discrete_m = 2` disjoint values,
#' the same binning options (specified by `breaks`) are available as in
#' [graphics::hist()]. Before calculating bins, outliers are capped via modified boxplot
#' rule using an IQR factor `outlier_iqr = 2` instead of 1.5.
#'
#' Note that partial dependence of numeric features is evaluated at (possibly weighted)
#' bin means, i.e., not at the bin center.
#'
#' @param object Fitted model.
#' @param v Vector of variable names to calculate statistics.
#' @param data Matrix or data.frame.
#' @param y Numeric vector with observed values of the response.
#'   Can also be a column name in `data`. Omitted if `NULL` (default).
#' @param pred Numeric vector with predictions. If `NULL`, it is calculated as
#'   `pred_fun(object, data, ...)`. Used to save time if `marginal()` is to be
#'   called multiple times.
#' @param pred_fun Prediction function, by default `stats::predict`.
#'   The function takes three arguments (names irrelevant): `object`, `data`, and `...`.
#' @param trafo How should predictions be transformed?
#'   A function or `NULL` (default). Examples are `log` (to switch to link scale)
#'   or `exp` (to switch from link scale to the original scale).
#' @param w Optional vector with case weights. Can also be a column name in `data`.
#' @param breaks An integer, vector, string or function specifying the bins
#'   of the numeric X variables as in [graphics::hist()]. The default is "Sturges".
#'   To allow varying values of `breaks` across variables, it can be a list of the
#'   same length as `v`, or a *named* list with `breaks` for certain variables.
#' @param right Should bins created via [graphics::hist()] be right-closed?
#'   The default is `TRUE`. Vectorized over `v`. Only relevant for numeric X.
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
#' @param ... Further arguments passed to `pred_fun()`, e.g., `type = "response"` in
#'   a `glm()` or (typically) `prob = TRUE` in classification models.
#' @returns
#'   A list (of class "marginal") with a data.frame of statistics per feature. Use
#'   single bracket subsetting to select part of the output.
#' @seealso [average_observed()], [partial_dependence()]
#' @export
#' @examples
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' xvars <- colnames(iris)[-1]
#' M <- marginal(fit, v = xvars, data = iris, y = "Sepal.Length", breaks = 5)
#' M
#' M |> postprocess(sort = TRUE) |> plot(share_y = TRUE)
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
    trafo = NULL,
    w = NULL,
    breaks = "Sturges",
    right = TRUE,
    discrete_m = 5L,
    outlier_iqr = 2,
    calc_pred = TRUE,
    pd_n = 500L,
    ...
) {
  # Input checks
  stopifnot(
    is.data.frame(data) || is.matrix(data),
    v %in% colnames(data),
    outlier_iqr >= 0,
    pd_n >= 0L
  )
  n <- nrow(data)
  nms <- colnames(data)
  stopifnot(
    n >= 2L,
    basic_check(w, n = n, nms = nms),
    basic_check(y, n = n, nms = nms),
    basic_check(pred, n = n, nms = nms)
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
    pred <- prep_pred(pred, trafo = trafo)
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
    pred <- prep_pred(pred_fun(object, data, ...), trafo = trafo)
  }

  PY <- cbind(pred = pred, y = y)  # cbind(NULL, NULL) gives NULL

  # Prepare pd_X and pd_w
  if (pd_n > 0L) {
    if (n > pd_n) {
      ix <- sample(n, pd_n)
      pd_X <- collapse::ss(data, ix)
      pd_w <- if (!is.null(w)) w[ix]
    } else {
      pd_X <- data
      pd_w <- w
    }
  } else {
    pd_X <- pd_w <- NULL
  }

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
      PY = PY,
      w = w,
      data = data,
      object = object,
      pred_fun = pred_fun,
      trafo = trafo,
      pd_X = pd_X,
      pd_w = pd_w,
      ...
    ),
    SIMPLIFY = FALSE
  )
  structure(out, class = "marginal")
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
    trafo = NULL,
    w = NULL,
    breaks = "Sturges",
    right = TRUE,
    discrete_m = 5L,
    outlier_iqr = 2,
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
    trafo = trafo,
    w = w,
    breaks = breaks,
    right = right,
    discrete_m = discrete_m,
    outlier_iqr = outlier_iqr,
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
  trafo = NULL,
  w = object[["weights"]],
  breaks = "Sturges",
  right = TRUE,
  discrete_m = 5L,
  outlier_iqr = 2,
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
    trafo = trafo,
    w = w,
    breaks = breaks,
    right = right,
    discrete_m = discrete_m,
    outlier_iqr = outlier_iqr,
    calc_pred = calc_pred,
    pd_n = pd_n,
    ...
  )
}

calculate_stats <- function(
    v,
    PY,
    w,
    data,
    breaks,
    right,
    discrete_m,
    outlier_iqr,
    object,
    pred_fun,
    trafo,
    pd_X,
    pd_w,
    ...
) {
  x <- if (is.matrix(data)) data[, v] else data[[v]]
  if (is.double(x)) {
    # {collapse} seems to distinguish positive and negative zeros
    # https://github.com/SebKrantz/collapse/issues/648
    # Adding 0 to a double turns negative 0 to positive ones (ISO/IEC 60559)
    collapse::setop(x, "+", 0.0)
  }
  g <- collapse::funique(x)

  # DISCRETE
  if (!is.numeric(x) || length(g) <= discrete_m) {
    num <- FALSE
    # Ordered by sort(g) (+ NA). For factors: levels(x) (+ NA)
    M <- grouped_stats(PY, g = x, w = w)
    g <- sort(g, na.last = TRUE)
    out <- data.frame(bin_mid = g, bin_width = 0.7, bin_mean = g, M)
    rownames(out) <- NULL
  } else {
    # "CONTINUOUS" case. Tricky because there can be empty bins.
    num <- TRUE
    if (outlier_iqr > 0 && is.finite(outlier_iqr)) {
      x <- wins_iqr(x, m = outlier_iqr)
    }
    br <- hist2(x, breaks = breaks)
    g <- 0.5 * (br[-1L] + br[-length(br)])  # mids
    gix <- seq_along(g)
    bin_width <- diff(br)
    if (anyNA(x)) {
      g <- c(g, NA)
      gix <- c(gix, NA)
      bin_width <- c(bin_width, NA)  #  Can't be plotted anyway
    }
    out <- data.frame(
      bin_mid = g, bin_width = bin_width, bin_mean = g, N = 0, weight = 0
    )

    # Integer encoding
    ix <- collapse::qF(
      findInterval(
        x, vec = br, rightmost.closed = TRUE, left.open = right, all.inside = TRUE
      ),
      sort = TRUE
    )
    M <- cbind(
      bin_mean = collapse::fmean.default(x, g = ix, w = w),
      grouped_stats(PY, g = ix, w = w)
    )
    reindex <- match(as.integer(rownames(M)), gix)
    out[reindex, colnames(M)] <- M  # Fill gaps
  }

  # Add partial dependence
  if (!is.null(pd_X)) {
    out[["pd"]] <- .pd(
      object = object,
      v = v,
      X = pd_X,
      grid = out[["bin_mean"]],
      pred_fun = pred_fun,
      trafo = trafo,
      w = pd_w,
      ...
    )
  }

  # Convert non-numeric levels *after* calculation of partial dependence!
  if (!num && !is.factor(out[["bin_mean"]])) {
    out[["bin_mid"]] <- out[["bin_mean"]] <- factor(out[["bin_mean"]])
  }
  return(out)
}
