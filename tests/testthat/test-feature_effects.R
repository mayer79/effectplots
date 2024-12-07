test_that("setting a seed works", {
  X <- data.frame(matrix(runif(1e5), ncol = 4))
  pf <- function(m, x) rowMeans(x)

  M1 <- feature_effects("anymodel", v = colnames(X), data = X, pred_fun = pf, seed = 7)
  M2 <- feature_effects("anymodel", v = colnames(X), data = X, pred_fun = pf, seed = 7)
  expect_equal(M1, M2)
})

test_that("discrete output keeps its type", {
  X <- transform(
    iris,
    char = rep(c("A", "B", "C"), times = 50),
    logical = Sepal.Width > median(Sepal.Width),
    int = cut(iris$Petal.Width, breaks = 0:3, labels = FALSE)
  )
  fit <- lm(Sepal.Length ~ Species + Petal.Length + char + logical + int, data = X)
  M <- feature_effects(
    fit, v = c("Species", "Petal.Length", "char", "logical", "int"), data = X
  )
  expect_equal(is_discrete(M), c(TRUE, FALSE, TRUE, TRUE, TRUE))
  expect_equal(
    vapply(M, function(z) typeof(z$bin_mid), FUN.VALUE = character(1), USE.NAMES = F),
    c("integer", "double", "character", "logical", "integer")
  )

  expect_no_error(plot(M))
})
