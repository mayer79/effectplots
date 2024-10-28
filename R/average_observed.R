#' Average Observed
#'
#' Calculates average observed `y` values over one or multiple `X` variables.
#' This function is a convenience wrapper around [marginal()].
#'
#' @param X A vector, matrix, or data.frame with variable(s) to be shown on the x axis.
#' @param y A numeric vector of observed responses.
#' @param w An optional numeric vector of weights.
#' @param x_name If `X` is a vector: what is the name of the variable? By default "x".
#' @inheritParams marginal
#' @inherit marginal return
#' @param ... Currently unused.
#' @seealso [marginal()]
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
    discrete_m = 5L,
    outlier_iqr = 2,
    ...
) {
  if (NCOL(X) == 1L && (is.vector(X) || is.factor(X))) {
    X <- stats::setNames(data.frame(X), x_name)
  }
  marginal.default(
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
    pd_n = 0L
  )
}

