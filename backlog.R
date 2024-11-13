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
IntegerVector findInterval3(
    NumericVector x, double low, double high, int nbin, bool right = true
) {
  const unsigned int n = x.size();
  IntegerVector out(n, NA_INTEGER);
  double D = (high - low) / nbin;

  for (int i = 0; i < n; i++) {
    double z = x[i];
    if (z <= low) {
      out[i] = 1;
    } else if (z >= high) {
      out[i] = nbin;
    } else if (!std::isnan(z)) {
      if (right) {
        out[i] = int(ceil((z - low) / D));
      } else {
        out[i] = int((z - low) / D) + 1;
      }
    }
  }
  return out;
}

            ")
x <- runif(1e7)
b1 <- 0
b2 <- 1
m <- 11L
breaks <- seq(0, 1, by = 0.1)
bench::mark(
  effectplots:::findInterval_equi(x, b1, b2, m, T), findInterval3(x, b1, b2, m, T)
)
