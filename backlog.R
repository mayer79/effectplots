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
  feature_effects(fit, v = v[1], data = X, calc_pred = FALSE), iterations = 2
)

# Matrix
n <- 1e7
X <- matrix(runif(n * 20), ncol = 20)
colnames(X) <- v <- paste0("V", 1:20)
y <- runif(n)

fit <- lm.fit(x = X, y = y)
pf <- function(m, x) c(tcrossprod(m$coefficients, x))
bench::mark(
  feature_effects(fit, v = v[1:5], data = X, calc_pred = FALSE, pred_fun = pf), iterations = 2
)

# New
339ms      2.95     356M  v[1]
1.57s     0.636    1.41GB v[1:5]
6.16s     0.162    4.64GB v

# Old
592ms      1.69    1.73GB v[1]
1.9s     0.527    2.34GB  v[5]
6.02s     0.166    4.64GB v[10]
