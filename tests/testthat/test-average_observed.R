test_that("average_observed() is consistent with marginal()", {
  fit <- lm(Sepal.Length ~ ., data = iris)
  v <- c("Sepal.Width", "Species")

  avg_obs <- average_observed(iris[v], y = iris$Sepal.Length, w = 1:150)
  marg <- marginal(
    fit,
    v = v,
    data = iris,
    y = iris$Sepal.Length,
    calc_pred = FALSE,
    pd_n = 0,
    ale_bin_size = 0,
    w = 1:150
  )
  expect_equal(avg_obs, marg)
})

test_that("single vector input works", {
  fit <- lm(Sepal.Length ~ ., data = iris)
  v <- c("Sepal.Width", "Species")

  out1 <- average_observed(iris$Species, y = iris$Sepal.Length)
  names(out1) <- "Species"
  out2 <- average_observed(iris$Species, y = iris$Sepal.Length, x_name = "Species")
  out3 <- average_observed(iris["Species"], y = iris$Sepal.Length)
  expect_equal(out1, out2)
  expect_equal(out1, out3)
})
