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
X <- qDF(matrix(rexp(n * 10), ncol = 10))
v <- colnames(X)
X$y <- runif(n)
fit <- lm(reformulate(v, "y"), data = X)
bench::mark(
  feature_effects(fit, v = v, data = X, calc_pred = FALSE, seed = 1),
  min_iterations = 3
)

# 1.38s     1.26GB

x <- runif(n)
bench::mark(frange(x))  # 8 ms
bench::mark(effectplots:::clamp2(x, 0.1, 0.9))  # 16 ms

# Discrete double -> sort = TRUE!   potential: 85% / 200ms
x <- sample(c(pi, exp(1), exp(-1)), n, TRUE)
x <- sample(c(0, 1, 2), n, TRUE)
bench::mark(qF(x))                # 417/204 ms
bench::mark(qF(x, sort=F))        # 21 ms
bench::mark(funique(x))           # 22 ms
bench::mark(fnunique(x))          # 19 ms

# Int (sort = TRUE)               potential 50% / 19ms
x <- sample(1:10, n, TRUE)
bench::mark(qF(x))                # 24 ms
bench::mark(qF(x, sort = FALSE))  # 11 ms
bench::mark(funique(x))           # 6 ms
bench::mark(fnunique(x))          # 8 ms

# Logical (sort does not matter)  potential 30% / 5ms
x <- sample(c(TRUE, FALSE), n, TRUE)
bench::mark(qF(x))                # 8 ms
bench::mark(qF(x, sort = FALSE))  # 8 ms
bench::mark(funique(x))           # 5 ms

# Char -> sort = FALSE  . potential: 65% / 21ms
x <- sample(letters[1:10], n, TRUE)
bench::mark(qF(x))                # 26 ms
bench::mark(qF(x, sort = FALSE))  # 12 ms
bench::mark(funique(x))           # 7 ms

# Factor -> sort = TRUE : potential: 100% / 5ms
x <- factor(sample(letters[1:10], n, TRUE))
bench::mark(qF(x))                # 0 ms
bench::mark(qF(x, sort = FALSE))  # 7 ms
bench::mark(funique(x))           # 5 ms


x <- qF(x, sort = is.factor(x))
# "factor", "double", "integer", "logical", "character"
orig_type <- if (is.factor(x)) "factor" else typeof(x)
rn <- c("3", "1", NA)
char2type <- function(x, type) {
  # we might lose precision with doubles -> impacts only partial dependence. But problem
  # only in pathetic cases, e.g., where c(pi, exp(1), ...) are integer encoded in model.
  switch(
    type,
    factor = factor(x, levels = stats::na.omit(x)),
    double = as.double(x),
    integer = as.integer(x),
    logical = as.logical(x),
    character = as.character(x)
  )
}
g <- char2type(rn, orig_type)

if (orig_type != "factor") {
  out <- out[order(g), ]
}


# Matrix
n <- 1e7
X <- matrix(rexp(n * 10), ncol = 10)
colnames(X) <- v <- paste0("V", 1:10)
y <- runif(n)

fit <- lm.fit(x = X, y = y)
pf <- function(m, x) c(tcrossprod(m$coefficients, x))
bench::mark(
  feature_effects(fit, v = v, data = X, calc_pred = FALSE, pred_fun = pf, seed = 1),
  iterations = 2
)

# New
# 1.72s    1.97GB
x <- sample(c(1:10, NA), 1e7, T)
f1  <- function(Y, x) {
  grouped_stats(Y, g = qF(x, sort=F), sd_cols = c("V1", "V2"))
}
f2 <- function(Y, x) {
  grouped_stats(Y, g = int2fact(x, 10), sd_cols = c("V1", "V2"))
}

Y <- qDF(matrix(rnorm(3e7), ncol = 3))
grouped_stats(Y, g = qF(x, sort=F), sd_cols = c("V1", "V2"))
grouped_stats(Y, g = int2fact(x, 10), sd_cols = c("V1", "V2"))
bench::mark(f1(Y, x), f2(Y, x), check=F, iterations = 10)

x <- runif(1e7)
br <- seq(0, 1, by = 0.05)
bench::mark(findInterval(x, br, T, T, left.open = T), findInterval2(x, br, right = T), iterations = 10)

#, spatstat.utils::fastFindInterval(x, br))

Rcpp::cppFunction("
NumericVector clamp(NumericVector x, double low, double high) {
  return clamp(low, x, high);
}
")
Rcpp::cppFunction("
NumericVector clamp2(NumericVector x, double low, double high) {
  for (int i = 0; i < x.size(); i++) {
    if (x[i] > high) {
      x[i] = high;
    } else if (x[i] < low) {
      x[i] = low;
    }
  }
  return x;
}
")


z <- sample(1:100, 1e7, T)
bench::mark(clamp3(z, 10L, 90L))
x <- runif(1e7)
bench::mark(clamp2(x, 0.1, 0.9))
b1 <- 0
b2 <- 1
m <- 11L
breaks <- seq(0, 1, by = 0.1)
bench::mark(
  effectplots:::findInterval_equi(x, b1, b2, m, T), findInterval3(x, b1, b2, m, T)
)

x <- sample(1:10, 1e7, T)
X <- as.data.frame(matrix(runif(2e7), ncol=2))
f1 <- function() {
  grouped_stats(X, g = qF(x, sort = FALSE))
}
f2 <- function() {
  grouped_stats(X, g = int2fact(x))
}
bench::mark(f1(), f2(), check=F)
