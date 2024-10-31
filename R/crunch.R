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
  g <- collapse::qF(g, sort = TRUE)
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
#' Very fast partial dependence calculations. Since the function does not do any
#' input checks, it is mainly meant for internal use. Still, for developers, it can
#' be handy.
#'
#' @param v Variable name in `data` to calculate partial dependence.
#' @param X Matrix or data.frame.
#' @param grid Vector or factor of values to calculate partial dependence for.
#' @param w Optional vector with case weights.
#' @inheritParams marginal
#' @returns Vector of partial dependence values in the same order as `grid`.
#' @export
#' @seealso [partial_dependence()]
#' @examples
#' fit <- lm(Sepal.Length ~ ., data = iris)
#'
#' .pd(fit, "Sepal.Width", X = iris, grid = hist(iris$Sepal.Width, plot = FALSE)$mids)
#' .pd(fit, "Species", X = iris, grid = levels(iris$Species))
.pd <- function(
    object, v, X, grid, pred_fun = stats::predict, trafo = NULL, w = NULL, ...
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
  pred <- prep_pred(pred_fun(object, X_pred, ...), trafo = trafo)
  return(wrowmean(pred, ngroups = p, w = w))
}
