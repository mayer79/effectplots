#' Average Observed
#'
#' Calculates average observed `y` values over one or multiple x variables specified
#' via `x_names`. `y` can either be a numeric vector or a column name in `data`.
#' This function is a convenience wrapper over [marginal()].
#'
#' @inheritParams marginal
#' @inherit marginal return
#' @param ... Currently unused.
#' @seealso [marginal()]
#' @export
#' @examples
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' M <- average_observed(
#'   x_name = "Sepal.Width", y = "Sepal.Length", data = iris, breaks = 5
#' )
#' M
#' M |> plot()
#'
#' xvars <- c("Sepal.Width", "Species")
#' M <- average_observed(xvars, data = iris, y = "Sepal.Length")
#' M$Species
#' plot(M, rotate_x = 45)
average_observed <- function(
    x_name,
    data,
    y,
    w = NULL,
    breaks = "Sturges",
    right = TRUE,
    discrete_m = 2L,
    winsorize_low = 0.01,
    winsorize_high = 0.99,
    ...
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
    winsorize_low = winsorize_low,
    winsorize_high = winsorize_high,
    calc_pred = FALSE,
    pd_n = 0L
  )
}

