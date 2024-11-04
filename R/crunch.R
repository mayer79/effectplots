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
#' @inheritParams marginal
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

# We don't need the following function, but it is easier to read than .ale()
# .ale_via_pd <- function(
#     object,
#     v,
#     X,
#     breaks,
#     right = TRUE,
#     pred_fun = stats::predict,
#     trafo = NULL,
#     which_pred = NULL,
#     bin_size = 200L,
#     w = NULL,
#     g = NULL,
#     ...
# ) {
#   if (is.null(g)) {
#     g <- collapse::qF(
#       findInterval(
#         if (is.data.frame(X)) X[[v]] else X[, v],
#         vec = breaks,
#         rightmost.closed = TRUE,
#         left.open = right,
#         all.inside = TRUE
#       ),
#       sort = TRUE
#     )
#   }
#   J <- lapply(
#     collapse::gsplit(1:length(g), g = g, use.g.names = TRUE),
#     function(z) if (length(z) <= bin_size) z else sample(z, size = bin_size)
#   )
#   if (is.na(names(J)[length(J)])) {
#     J <- J[-length(J)]
#   }
#
#   out <- numeric(length(breaks) - 1L)
#   for (nm in names(J)) {
#     j <- as.integer(nm)
#     pdj <- .pd(
#       object,
#       v = v,
#       X = collapse::ss(X, J[[nm]]),
#       grid = breaks[c(j, j + 1L)],
#       pred_fun = pred_fun,
#       trafo = trafo,
#       which_pred = which_pred,
#       w = if (!is.null(w)) w[J[[nm]]],
#       ...
#     )
#     out[j] <- diff(pdj)
#   }
#   return(cumsum(out))
# }

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
#' @param g For internal use. The result of `factor(findInterval(...))`.
#'   By default `NULL`.
#' @inheritParams marginal
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
    g <- collapse::qF(
      findInterval(
        if (is.data.frame(data)) data[[v]] else data[, v],
        vec = breaks,
        rightmost.closed = TRUE,
        left.open = right,
        all.inside = TRUE
      ),
      sort = TRUE
    )
  }

  # List of bin indices. Eventual NA levels are placed at the end. We will remove it.
  J <- lapply(
    collapse::gsplit(1:length(g), g = g, use.g.names = TRUE),
    function(z) if (length(z) <= bin_size) z else sample(z, size = bin_size)
  )
  if (is.na(names(J)[length(J)])) {
    J <- J[-length(J)]
  }

  # Before flattening the list J, we store bin counts (with bin names)
  bin_n <- lengths(J)
  J <- unlist(J, recursive = FALSE, use.names = FALSE)

  # Empty bins will get an incremental effect of 0
  p <- length(breaks) - 1L
  out <- numeric(p)
  ok <- (1L:p) %in% as.integer(names(bin_n))

  # Now we create a single prediction dataset. Lower bin edges first, then upper ones.
  data_long <- collapse::ss(data, rep.int(J, 2L))
  grid_long <- rep.int(
    c(breaks[-(p + 1L)][ok], breaks[-1L][ok]), times = c(bin_n, bin_n)
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
  out[ok] <- collapse::fmean(
    pred[(n + 1L):(2L * n)] - pred[1L:n], g = g[J], w = if (!is.null(w)) w[J]
  )
  return(cumsum(out))
}
