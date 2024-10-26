#' Marginal Statistics
#'
#' This is the main function of the package. It calculates
#' - average observed,
#' - average predicted,
#' - partial dependence, and
#' - counts/weights
#' over (possibly binned) features X specified by their column names `v`.
#' Furthermore, standard deviations (without -1 in the denominator) are calculated
#' for observed and predicted values.
#'
#' For numeric variables with more than `discrete_m = 2` disjoint values,
#' the same binning options (specified by `breaks`) are available as in
#' [graphics::hist()]. Before calculating bins, the smallest and largest 1% of the
#' values are winsorized (capped) at the corresponding observed (approximate) quantiles.
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
#' @param w Optional vector with case weights. Can also be a column name in `data`.
#' @param breaks An integer, vector, string or function specifying the bins
#'   of the numeric X variables as in [graphics::hist()]. The default is "Sturges".
#'   To allow varying values of `breaks` across variables, it can be a list of the
#'   same length as `v`, or a *named* list with `breaks` for certain variables.
#' @param right Should bins created via [graphics::hist()] be right-closed?
#'   The default is `TRUE`. Vectorized over `v`. Only relevant for numeric X.
#' @param discrete_m Numeric X variables with up to this number of unique values
#'   should not be binned. The default is 2. Vectorized over `v`.
#' @param wprob_low The smallest values of a numeric X are capped at the corresponding
#'   quantile. Set to 0 to avoid Winsorizing. Note that at most 100k observations
#'   are sampled to calculate the quantile (uses random seed). Vectorized over `v`.
#' @param wprob_high The largest values of a numeric X are capped at the corresponding
#'   quantile. Set to 1 to avoid Winsorizing. Note that at most 100k observations
#'   are sampled to calculate the quantile (uses random seed). Vectorized over `v`.
#' @param calc_pred Should predictions be calculated? Default is `TRUE`. Only relevant
#'   if `pred = NULL`.
#' @param pd_n Size of the data used for calculating partial dependence.
#'   The default is 500. For larger `data` (and `w`), `pd_n` rows are randomly sampled
#'   (uses random seed). Each variable specified by `v` uses the same subsample.
#'   Set to 0 to omit.
#' @param ... Further arguments passed to `pred_fun()`, e.g., `type = "response"` in
#'   a `glm()` or (typically) `prob = TRUE` in binary probabilistic models.
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
#' M |> postprocess(sort = TRUE) |> plot()
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
    wprob_low = 0.01,
    wprob_high = 0.99,
    calc_pred = TRUE,
    pd_n = 500L,
    ...
) {
  stopifnot(
    is.data.frame(data) || is.matrix(data),
    is.function(pred_fun),
    v %in% colnames(data),
    wprob_low <= wprob_high
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

  # Prepare pd_X and pd_w
  if (pd_n > 0L) {
    if (nrow(data) > pd_n) {
      ix <- sample(nrow(data), pd_n)
      pd_X <- data[ix, , drop = FALSE]
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
    wprob_low = wprob_low,
    wprob_high = wprob_high,
    MoreArgs = list(
      pred = pred,
      y = y,
      w = w,
      data = data,
      object = object,
      pred_fun = pred_fun,
      pd_X = pd_X,
      pd_w = pd_w,
      ...
    ),
    SIMPLIFY = FALSE
  )
  class(out) <- "marginal"
  return(out)
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
    wprob_low = 0.01,
    wprob_high = 0.99,
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
    wprob_low = wprob_low,
    wprob_high = wprob_high,
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
  wprob_low = 0.01,
  wprob_high = 0.99,
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
    wprob_low = wprob_low,
    wprob_high = wprob_high,
    calc_pred = calc_pred,
    pd_n = pd_n,
    ...
  )
}

#' @export
print.marginal <- function(x, ...) {
  cat(
    "'marginal' object of length ",
    length(x),
    if (length(x) > 1) paste0(", starting with '", names(x)[1L], "'") else "",
    ": \n\n",
    sep = ""
  )
  print(x[[1L]])
  invisible(x)
}

#' @export
`[.marginal` <- function(x, ...) {
  structure(NextMethod(), class = "marginal")
}

calculate_stats <- function(
    v,
    pred,
    y,
    w,
    data,
    breaks,
    right,
    discrete_m,
    wprob_low,
    wprob_high,
    object,
    pred_fun,
    pd_X,
    pd_w,
    ...
) {

  # Prepare x
  x <- if (is.matrix(data)) data[, v] else data[[v]]
  if (is.numeric(x) && (wprob_low > 0 || wprob_high < 1)) {
    x <- wins_prob(x, probs = c(wprob_low, wprob_high), nmax = 1e5)
  }

  g <- unique(x)  # Could be avoided for factors - but would need to keep track of NA

  # DISCRETE
  if (!is.numeric(x) || length(g) <= discrete_m) {
    num <- FALSE
    # Ordered by sort(g) (+ NA). For factors, this equals levels(g) (+ NA)
    M <- grouped_mean(cbind(pred = pred, obs = y), g = x, w = w)
    # S <- grouped_sd(cbind(pred = pred, obs = y), g = x, M = M, w = w)
    g <- sort(g, na.last = TRUE)
    out <- data.frame(bar_at = g, bar_width = 0.7, eval_at = g, M)
    rownames(out) <- NULL
  } else {
    # "CONTINUOUS" case. Tricky because there can be empty bins.
    num <- TRUE
    H <- hist2(x, breaks = breaks)
    g <- H$mids
    gix <- seq_along(g)
    bar_width <- diff(H$breaks)
    if (anyNA(x)) {
      g <- c(g, NA)
      gix <- c(gix, NA)
      bar_width <- c(bar_width, NA)
    }
    out <- data.frame(bar_at = g, bar_width = bar_width, eval_at = g, exposure = 0)

    # Integer encoding
    ix <- findInterval(
      x, vec = H$breaks, rightmost.closed = TRUE, left.open = right, all.inside = TRUE
    )
    M <- grouped_mean(cbind(eval_at = x, pred = pred, obs = y), g = ix, w = w)
    # M <- cbind(M, grouped_sd(cbind(pred = pred, obs = y), g = ix, M = M, w = w))
    reindex <- match(as.integer(rownames(M)), gix)
    out[reindex, colnames(M)] <- M  # Fill gaps
  }

  # Add partial dependence
  if (!is.null(pd_X)) {
    out[["pd"]] <- partial_dep(
      object = object,
      v = v,
      X = pd_X,
      grid = out[["eval_at"]],
      pred_fun = pred_fun,
      w = pd_w,
      ...
    )
  }

  # Convert non-numeric levels *after* calculation of partial dependence!
  if (!num && !is.factor(out[["eval_at"]])) {
    out[["bar_at"]] <- out[["eval_at"]] <- factor(out[["eval_at"]])
  }
  return(out)
}
