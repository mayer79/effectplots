#' Average Observed
#'
#' Calculates average observed `y` values over one or multiple X variables `v`.
#' The `y` argument can either be a numeric vector or a column name in `data`.
#' This function is a convenience wrapper around [marginal()].
#'
#' @inheritParams marginal
#' @inherit marginal return
#' @param ... Currently unused.
#' @seealso [marginal()]
#' @export
#' @examples
#' M <- average_observed(v = "Species", y = "Sepal.Length", data = iris)
#' M
#' M |> plot()
average_observed <- function(
    v,
    data,
    y,
    w = NULL,
    breaks = "Sturges",
    right = TRUE,
    discrete_m = 5L,
    outlier_iqr = 2,
    ...
) {
  marginal.default(
    object = NULL,
    v = v,
    data = data,
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

