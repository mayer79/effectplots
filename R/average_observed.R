#' Average Observed
#'
#' Calculates average observed response over the values of one or multiple
#' variables specified by `X`. This describes the statistical association between the
#' response `y` and potential model features.
#'
#' The function is a convenience wrapper around [feature_effects()].
#'
#' @param X A vector, matrix, or data.frame with features.
#' @param y A numeric vector representing observed response values.
#' @param w An optional numeric vector of weights. Having observations with
#'   non-positive weight is equivalent to excluding them.
#' @param x_name If `X` is a vector: what is the name of the variable? By default "x".
#' @param seed Optional integer random seed used for calculating breaks:
#'   The bin range is determined without values outside quartiles +- 2 IQR
#'   using a sample of <= 9997 observations to calculate quartiles.
#' @inheritParams feature_effects
#' @inherit feature_effects return
#' @param ... Currently unused.
#' @seealso [feature_effects()]
#' @export
#' @examples
#' M <- average_observed(iris$Species, y = iris$Sepal.Length)
#' M
#' M |> plot()
#'
#' # Or multiple potential features X
#' average_observed(iris[2:5], y = iris[, 1], breaks = 5) |>
#'   plot()
average_observed <- function(
    X,
    y,
    w = NULL,
    x_name = "x",
    breaks = "Sturges",
    right = TRUE,
    discrete_m = 13L,
    outlier_iqr = 2,
    seed = NULL,
    ...
) {
  if (NCOL(X) == 1L && (is.vector(X) || is.factor(X))) {
    X <- collapse::frename(collapse::qDF(X), x_name)
  }
  stopifnot(
    is.matrix(X) || is.data.frame(X),
    length(y) == nrow(X),
    is.null(w) || length(w) == nrow(X)
  )
  feature_effects.default(
    object = NULL,
    v = colnames(X),
    data = X,
    y = y,
    w = w,
    breaks = breaks,
    right = right,
    discrete_m = discrete_m,
    outlier_iqr = outlier_iqr,
    calc_pred = FALSE,
    pd_n = 0L,
    ale_n = 0L,
    seed = seed
  )
}

