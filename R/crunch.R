wins_iqr <- function(x, m = 1.5, nmax = 10000L) {
  xs <- if (length(x) > nmax) sample(x, nmax) else x
  q <- collapse::fquantile(xs, probs = c(0.25, 0.75), na.rm = TRUE, names = FALSE)
  r <- m * diff(q)
  if (r <= 0) {
    return(x)
  }
  winsorize(x, low = q[1L] - r, high = q[2L] + r)
}

# pmin() and pmax() are surprisingly fast for large vectors
# and need less memory than subsetting
winsorize <- function(x, low = -Inf, high = Inf) {
  r <- collapse::.range(x, na.rm = TRUE)
  if (r[1L] < low) {
    x <- pmax(x, low)
  }
  if (r[2L] > high) pmin(x, high) else x
}

# Later replace by collapse::qsu()
grouped_stats <- function(x, g, w = NULL) {
  # returns rows in order sort(unique(x)) + NA, or levels(x) + NA (if factor)
  if (!is.factor(g)) {
    g <- collapse::qF(g, sort = TRUE)
  }
  N <- collapse::fsum.default(rep.int(1L, times = length(g)), g = g)
  if (!is.null(w)) {
    weight <- collapse::fsum.default(w, g = g)
  } else {
    weight <- N
  }
  out <- cbind(N = N, weight = weight)
  if (is.null(x)) {
    return(out)
  }
  M <- collapse::fmean.matrix(x, g = g, w = w, use.g.names = FALSE)
  S <- collapse::fsd.matrix(x, g = g, w = w, use.g.names = FALSE)
  colnames(M) <- paste0(colnames(S), "_mean")
  colnames(S) <- paste0(colnames(S), "_sd")
  return(cbind(out, M, S))
}

wrowmean <- function(x, ngroups = 1L, w = NULL) {
  dim(x) <- c(length(x) %/% ngroups, ngroups)
  if (is.null(w)) colMeans(x) else colSums(x * w) / sum(w)
}

#' Barebone Partial Dependence
#'
#' This is a barebone implementation of Friedman's partial dependence
#' intended for developers.
#'
#' @param v Variable name in `data` to calculate partial dependence.
#' @param X Matrix or data.frame.
#' @param grid Vector or factor of values to calculate partial dependence for.
#' @param w Optional vector with case weights.
#' @inheritParams marginal
#' @returns Vector of partial dependence values in the same order as `grid`.
#' @export
#' @seealso [partial_dependence()]
#' @inherit partial_dependence references
#' @examples
#' fit <- lm(Sepal.Length ~ ., data = iris)
#'
#' .pd(fit, "Sepal.Width", X = iris, grid = hist(iris$Sepal.Width, plot = FALSE)$mids)
#' .pd(fit, "Species", X = iris, grid = levels(iris$Species))
.pd <- function(
    object,
    v,
    X,
    grid,
    pred_fun = stats::predict,
    trafo = NULL,
    which_pred = NULL,
    w = NULL,
    ...
) {
  n <- nrow(X)
  p <- length(grid)
  X_pred <- collapse::ss(X, rep.int(seq_len(n), p))
  grid_pred <- rep(grid, each = n)
  if (is.data.frame(X_pred)) {
    X_pred[[v]] <- grid_pred
  } else {
    X_pred[, v] <- grid_pred
  }
  pred <- prep_pred(
    pred_fun(object, X_pred, ...), trafo = trafo, which_pred = which_pred
  )
  return(wrowmean(pred, ngroups = p, w = w))
}

#' Barebone Accumulated Local Effects
#'
#' Very fast ALE calculations. Since the function does not do any
#' input checks, it is mainly meant for internal use. Still, for developers, it can
#' be handy.
#'
#' @param v Variable name in `data` to calculate ALE.
#' @param X Matrix or data.frame.
#' @param breaks Breaks for ALE calculation.
#' @param right Should bins specified via `breaks` be right-closed?
#'   The default is `TRUE`.
#' @param bin_size Maximal number of observations used per bin. If there are more
#'   observations in a bin, `bin_size` indices are randomly sampled. The default is 200.
#' @param w Optional vector with case weights.
#' @param g For internal use. The result of `factor(findInterval(...))`.
#'   By default `NULL`.
#' @inheritParams marginal
#' @returns Vector of ALE values in the same order as `breaks[-length(breaks)]`.
#' @export
#' @seealso [partial_dependence()]
#' @references
#'   Apley, Daniel W., and Jingyu Zhu. 2016. *Visualizing the Effects of Predictor Variables in Black Box Supervised Learning Models.*
#'     Journal of the Royal Statistical Society Series B: Statistical Methodology,
#'     82 (4): 1059–1086. doi:10.1111/rssb.12377.
#' @examples
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' v <- "Sepal.Width"
#' .ale(fit, v, X = iris, breaks = seq(2, 4, length.out = 5))
.ale <- function(
    object,
    v,
    X,
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
    g <- collapse::qF(
      findInterval(
        if (is.data.frame(X)) X[[v]] else X[, v],
        vec = breaks,
        rightmost.closed = TRUE,
        left.open = right,
        all.inside = TRUE
      ),
      sort = TRUE
    )
  }
  J <- lapply(
    collapse::gsplit(1:length(g), g = g, use.g.names = TRUE),
    function(z) if (length(z) <= bin_size) z else sample(z, size = bin_size)
  )
  if (is.na(names(J)[length(J)])) {
    J <- J[-length(J)]
  }

  out <- numeric(length(breaks) - 1L)
  for (j in as.integer(names(J))) {
    pdj <- .pd(
      object,
      v = v,
      X = collapse::ss(X, J[[j]]),
      grid = breaks[c(j, j + 1L)],
      pred_fun = pred_fun,
      trafo = trafo,
      which_pred = which_pred,
      w = if (!is.null(w)) w[J[[j]]],
      ...
    )
    out[j] <- diff(pdj)
  }
  return(cumsum(out))
}
