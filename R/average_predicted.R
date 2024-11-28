#' Average Predictions
#'
#' Calculates average predictions over the values of one or multiple
#' `X` variables. Shows the combined effect of a feature and other (correlated)
#' features.
#'
#' The function is a convenience wrapper around [feature_effects()].
#'
#' @param pred A numeric vector of predictions.
#' @param x_name If `X` is a vector: what is the name of the variable? By default "x".
#' @inheritParams average_observed
#' @inheritParams feature_effects
#' @inherit feature_effects return
#' @param ... Currently unused.
#' @seealso [feature_effects()]
#' @references
#'   Apley, Daniel W., and Jingyu Zhu. 2016. *Visualizing the Effects of Predictor Variables in Black Box Supervised Learning Models.*
#'     Journal of the Royal Statistical Society Series B: Statistical Methodology,
#'     82 (4): 1059–1086. doi:10.1111/rssb.12377.
#' @export
#' @examples
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' M <- average_predicted(iris[2:5], pred = predict(fit, iris), breaks = 5)
#' M
#' M |> plot()
average_predicted <- function(
    X,
    pred,
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
  feature_effects.default(
    object = NULL,
    v = colnames(X),
    data = X,
    pred = pred,
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

