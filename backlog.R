library(effectplots)
library(dplyr)
library(data.table)
library(profvis)
library(collapse)

fit <- lm(Sepal.Length ~ ., data = iris)
xvars <- colnames(iris)[-1]

feature_effects(fit, v = xvars, data = as_tibble(iris), y = "Sepal.Length", breaks = 5)
feature_effects(fit, v = xvars, data = as.data.table(iris), y = "Sepal.Length", breaks = 5)


n <- 1e4
X <- qDF(matrix(runif(n * 10), ncol = 10))
v <- colnames(X)
X$y <- runif(n)

fit <- lm(reformulate(v, "y"), data = X)
bench::mark(
  feature_effects(fit, v = v, data = X, calc_pred = FALSE), iterations = 100
)

profvis(feature_effects(fit, v = v, data = X, calc_pred = FALSE))

6.4 GB, 6.13s
5 GB, 5.84s


33ms    58.6MB
40ms    58.8MB
