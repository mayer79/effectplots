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

X <- qDF(matrix(rnorm(1e7 * 10), ncol = 10))
# X <- qDF(matrix(sample(c(0, 1), 1e7 * 10, TRUE), ncol = 10))
# X <- qDF(matrix(sample(letters[1:4], 1e7 * 10, TRUE), ncol = 10))
# X[] <- lapply(X, factor)

v <- colnames(X)
X$y <- runif(n)
fit <- lm(reformulate(v, "y"), data = X)
bench::mark(
  feature_effects(fit, v = v, data = X, calc_pred = FALSE, seed = 1),
  min_iterations = 3
)

# 0.7s      593 MB  # rnorm
# 0.4s      504 MB  # sample(c(0, 1))
# 0.2s      502 MB  # sample(0:1)
# 0.2s      528 MB  # sample(letters[1:4])
# 0.2s      139 MB  # factor(letters[1:4])

# Matrix
n <- 1e7
X <- matrix(rnorm(n * 10), ncol = 10)
colnames(X) <- v <- paste0("V", 1:10)
y <- runif(n)

fit <- lm.fit(x = X, y = y)
pf <- function(m, x) c(tcrossprod(m$coefficients, x))
bench::mark(
  feature_effects(fit, v = v, data = X, calc_pred = FALSE, pred_fun = pf, seed = 1),
  iterations = 2
)

# New
# 0.9s    1.27GB

x <- runif(n)
bench::mark(frange(x)) # 8 ms

# Discrete double -> sort = TRUE!   potential: 85% / 200ms
x <- sample(c(pi, exp(1), exp(-1)), n, TRUE)
x <- sample(c(0, 1, 2), n, TRUE)
bench::mark(qF(x)) # 417/204 ms
bench::mark(qF(x, sort = F)) # 21 ms
bench::mark(funique(x)) # 22 ms
bench::mark(fnunique(x)) # 19 ms

# Int (sort = TRUE)               potential 50% / 19ms
x <- sample(1:10, n, TRUE)
bench::mark(qF(x)) # 24 ms
bench::mark(qF(x, sort = FALSE)) # 11 ms
bench::mark(funique(x)) # 6 ms
bench::mark(fnunique(x)) # 8 ms

# Logical (sort does not matter)  potential 30% / 5ms
x <- sample(c(TRUE, FALSE), n, TRUE)
bench::mark(qF(x)) # 8 ms
bench::mark(qF(x, sort = FALSE)) # 8 ms
bench::mark(funique(x)) # 5 ms

# Char -> sort = FALSE  . potential: 65% / 21ms
x <- sample(letters[1:10], n, TRUE)
bench::mark(qF(x)) # 26 ms
bench::mark(qF(x, sort = FALSE)) # 12 ms
bench::mark(funique(x)) # 7 ms

bench::mark(qF(x, sort = FALSE, na.exclude = FALSE))

# Factor -> sort = TRUE : potential: 100% / 5ms
x <- factor(sample(letters[1:10], n, TRUE))
bench::mark(qF(x)) # 0 ms
bench::mark(qF(x, sort = FALSE)) # 7 ms
bench::mark(funique(x)) # 5 ms
