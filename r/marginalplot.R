#' Marginal Statistics
#'
#' Calculates
#' - average observed,
#' - average predicted,
#' - partial dependence, and
#' - counts/weights
#' over a (binned) feature v, possibly weighted.
#'
#' @param object Fitted model.
#' @param x_name Column name of the stratification variable shown on the x axis.
#' @param data Matrix-like.
#' @param y Numeric vector with observed values of the response.
#'   Can also be a column name in `data`. Omitted if `NULL` (default).
#' @param pred Numeric vector with predictions. If `NULL`, it is calculated as
#'   `pred_fun(object, data, ...)`. Used to save time if the function is to be
#'   called multiple times.
#' @param pred_fun Prediction function, by default `stats::predict`.
#'   The function takes three arguments (names irrelevant): `object`, `data`, and `...`.
#' @param w Optional vector with case weights. Can also be a column name in `data`.
#' @param breaks An integer, a vector, a string or a function specifying the bins
#'   of `x`, and passed to [graphics::hist()]. The default is "Sturges".
#'   *Not* vectorized over `x_name`.
#' @param right Should bins created via [graphics::hist()] be right-closed?
#'   The default is `TRUE`. Vectorized over `x_name`.
#' @param discrete_m Numeric variable with up to this number of unique values
#'   should not be binned. The default is 2. Set to `Inf` to avoid any binning.
#'   Vectorized over `x_name`.
#' @param wins_low Small values of `x` are capped at the `wins_low` quantile.
#'   Set to 0 to avoid Winsorizing. Note that at most 100k observations are sampled
#'   to calculate the quantile. Vectorized over `x_name`.
#' @param wins_low High values of `x` are capped at the `wins_high` quantile.
#'   Set to 1 to avoid Winsorizing. Note that at most 100k observations are sampled
#'   to calculate the quantile. Vectorized over `x_name`.
#' @param calc_pred Should predictions `pred` be calculated? Default is `TRUE`.
#' @param pd_n Size of the dataset used for calculation of partial dependence.
#'   The default is 500. Set to 0 (or pass `pred_fun = NULL`) to omit calculation
#'   of partial dependence.
#' @param ... Further arguments passed to `pred_fun()`, e.g., `type = "response"` in
#'   a `glm()` model.
#' @returns
#'   An object of class "marginal" containing these elements:
#'   - `data`: data.frame containing the partial dependencies.
#'   - `v`: Same as input `v`.
#'   - `K`: Number of columns of prediction matrix.
#'   - `pred_names`: Column names of prediction matrix.
#'   - `by_name`: Column name of grouping variable (or `NULL`).
#' @references
#'   Friedman, Jerome H. *"Greedy Function Approximation: A Gradient Boosting Machine."*
#'     Annals of Statistics 29, no. 5 (2001): 1189-1232.
#' @export
#' @examples
#' library(ranger)
#'
#' fit <- ranger(Sepal.Length ~ ., data = iris)
#' pf <- function(m, x) predict(m, x)$predictions
#'
#' M <- marginal.default(fit, x_name = c("Sepal.Width", "Petal.Width", "Petal.Length", "Species"), data = iris, y = "Sepal.Length", pred_fun = pf)
#' M$Petal.Width
#' M |> plot()
#' M |> plot(backend = "plotly")
marginal <- function(object, ...) {
  UseMethod("marginal")
}

#' @describeIn marginal Default method.
#' @export
marginal.default <- function(
    object,
    x_name,
    data,
    y = NULL,
    pred = NULL,
    pred_fun = stats::predict,
    w = NULL,
    breaks = "Sturges",
    right = TRUE,
    discrete_m = 2L,
    wins_low = 0.01,
    wins_high = 0.99,
    calc_pred = TRUE,
    pd_n = 500L,
    ...
) {
  stopifnot(
    is.data.frame(data) || is.matrix(data),
    is.function(pred_fun),
    x_name %in% colnames(data),
    wins_low <= wins_high
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

  # Deal with multiple x_name
  if (length(x_name) > 1L) {
    out <- mapply(
      x_name,
      FUN = marginal,
      right = right,
      discrete_m = discrete_m,
      wins_low = wins_low,
      wins_high = wins_high,
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
    names(out) <- x_name
    class(out) <- "multimarginal"
    return(out)
  }

  # Prepare x
  x <- if (is.matrix(data)) data[, x_name] else data[[x_name]]
  if (is.numeric(x) && (wins_low > 0 || wins_high < 1)) {
    x <- winsorize(x, probs = c(wins_low, wins_high), nmax = 1e5)
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
      v = x_name,
      X = data,
      grid = out$data$eval_at,
      pred_fun = pred_fun,
      pd_n = pd_n,
      w = w,
      ...
    )
  }

  if (out$discrete && !is.factor(out$data$bar_at)) {
    # To ensure nice plot scales
    out$data$bar_at <- out$data$eval_at <- factor(out$data$bar_at)
  }
  out$x_name <- x_name
  class(out) <- "marginal"
  out
}

#' @describeIn marginal Method for "ranger" models.
#' @export
# marginal.ranger <- function() {
#   print("Todo")
#   # marginal.default()
# }

#' @describeIn marginal Method for DALEX "explainer".
#' @export
marginal.explainer <- function() {
  print("Todo")
  # marginal.default()
}

#' @inheritParams marginal
#' @export
#' @examples
#' M <- average_observed(x_name = "Species", data = iris, y = "Sepal.Length")
#' M
#' M |> plot()
#' M |> plot(backend = "plotly")
#'
#' xvars <- c("Sepal.Width", "Species")
#' M <- average_observed(xvars, data = iris, y = "Sepal.Length")
#' M
#' M$Species
#' plot(M)
average_observed <- function(
    x_name, data, y, w = NULL, breaks = "Sturges", right = TRUE, discrete_m = 2L
) {
  marginal.default(
    object = NULL,
    x_name = x_name,
    data = data,
    y = y,
    w = w,
    breaks = breaks,
    right = right,
    discrete_m = discrete_m,
    calc_pred = FALSE,
    pd_n = 0L
  )
}

#' @inheritParams marginal
#' @export
#' @examples
#' M <- partial_dependence(fit, x_name = "Species", data = iris, pred_fun = pf)
#' M
#' M |> plot()
#' M |> plot(backend = "plotly")
#'
#' xvars <- c("Species", "Sepal.Width")
#' M <- partial_dependence(fit, x_name = xvars, data = iris, pred_fun = pf)
#' M
#' M$Species
#' plot(M)
partial_dependence <- function(
    object,
    x_name,
    data,
    pred_fun = stats::predict,
    w = NULL,
    breaks = "Sturges",
    right = TRUE,
    discrete_m = 2L,
    pd_n = 500L
) {
  marginal.default(
    object = object,
    x_name = x_name,
    data = data,
    y = NULL,
    pred_fun = pred_fun,
    w = w,
    breaks = breaks,
    right = right,
    discrete_m = discrete_m,
    calc_pred = FALSE,
    pd_n = pd_n
  )
}

#' @export
print.marginal <- function(x, ...) {
  cat("marginal object: \n", sep = "")
  print(x$data)
  invisible(x)
}

#' @export
print.multimarginal <- function(x, ...) {
  cat("multimarginal object of length", length(x), "\n")
  invisible(x)
}

postprocess <- function(object, ...) {
  UseMethod("postprocess")
}

postprocess.default <- function(object, ...) {
  stop("Undefined")
}

postprocess.marginal <- function(
    object,
    disc_drop_below_n = 0,
    disc_drop_below_prop = 0,
    disc_explicit_na = TRUE,
    drop_na = FALSE
  ) {
  X <- object$data

  if (object$discrete) {
    if (disc_drop_below_n > 0) {
      X <- X[X$exposure >= drop_below_n, ]
    }
    if (disc_drop_below_prop > 0) {
      X <- X[X$exposure / sum(X$exposure)>= drop_below_prop, ]
    }

    if (disc_explicit_na) {
      s <- is.na(X$bar_at)
      if (any(s)) {
        lvl <- levels(X$bar_at)
        if ("NA" %in% lvl) {
          stop("'bar_at' contains level 'NA'. This is incompatible with missing handling.")
        }
        levels(X$bar_at) <- levels(X$eval_at) <- c(lvl, "NA")
        X[s, c("bar_at", "eval_at")] <- "NA"
      }
    }
    X <- droplevels(X)
  } else {
    if (isTRUE(drop_na)) {
      X <- X[!is.na(X$bar_at), ]
    }
  }

  object$data <- X
  return(object)
}

postprocess.multimarginal <- function(object, ...) {
  out <- lapply(object, postprocess)
  class(out) <- "multimarginal"
}
