library(effectplots)
library(dplyr)
library(data.table)
library(profvis)
library(collapse)

fit <- lm(Sepal.Length ~ ., data = iris)
xvars <- colnames(iris)[-1]

feature_effects(fit, v = xvars, data = as_tibble(iris), y = "Sepal.Length", breaks = 5)
feature_effects(fit, v = xvars, data = as.data.table(iris), y = "Sepal.Length", breaks = 5)


n <- 1e7
X <- qDF(matrix(runif(n * 10), ncol = 10))
v <- colnames(X)
X$y <- runif(n)

fit <- lm(reformulate(v, "y"), data = X)
bench::mark(
  feature_effects(fit, v = v, data = X, calc_pred = FALSE, seed = 1),
  iterations = 2
)

# 2.87s     1.63GB

# Matrix
n <- 1e7
X <- matrix(runif(n * 10), ncol = 10)
colnames(X) <- v <- paste0("V", 1:10)
y <- runif(n)

fit <- lm.fit(x = X, y = y)
pf <- function(m, x) c(tcrossprod(m$coefficients, x))
bench::mark(
  feature_effects(fit, v = v, data = X, calc_pred = FALSE, pred_fun = pf, seed = 1),
  iterations = 2
)

# New
# 3.07s    2.35GB

f1  <- function(pred, y) {
  cbind(pred = pred, y = y, resid = if (!is.null(pred) && !is.null(y)) y - pred)
}
f2 <- function(pred = NULL, y = NULL) {
  PYR <- list(pred = pred, y = y, resid = if (!is.null(pred) && !is.null(y)) y - pred)
  qDF(PYR[lengths(PYR) > 0L])
}
