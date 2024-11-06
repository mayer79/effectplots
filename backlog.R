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
  feature_effects(fit, v = v, data = X, calc_pred = FALSE), iterations = 2
)

5.66s     0.177    4.98GB

# Matrix
298ms      3.23    75.3MB  (dev)
299ms      3.12    90.6MB  (package)

# DF
593ms      1.68     572MB (package)
558ms      1.74     572MB
5.66s     0.177    4.98GB

570ms      1.75     573MB (dev)
572ms      1.73     573MB
5.68s     0.176    4.98GB
