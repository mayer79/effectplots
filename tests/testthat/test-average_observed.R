fit <- lm(Sepal.Length ~ ., data = iris)
v <- c("Sepal.Width", "Species")

test_that("average_observed() is consistent with feature_effects()", {
  avg_obs <- average_observed(iris[v], y = iris$Sepal.Length)
  marg <- feature_effects(
    fit,
    v = v,
    data = iris,
    y = iris$Sepal.Length,
    calc_pred = FALSE,
    pd_n = 0,
    ale_n = 0
  )
  expect_equal(avg_obs, marg)
})

test_that("single vector input works", {
  out1 <- average_observed(iris$Species, y = iris$Sepal.Length)
  names(out1) <- "Species"
  out2 <- average_observed(iris$Species, y = iris$Sepal.Length, x_name = "Species")
  out3 <- average_observed(iris["Species"], y = iris$Sepal.Length)
  expect_equal(out1, out2)
  expect_equal(out1, out3)
})
