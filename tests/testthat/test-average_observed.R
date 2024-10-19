test_that("average_observed() is consistent with marginal()", {
  fit <- lm(Sepal.Length ~ ., data = iris)
  v <- c("Sepal.Width", "Species")

  avg_obs <- average_observed(
    v = v,
    data = iris,
    y = iris$Sepal.Length,
    winsorize_low = 0.1,
    winsorize_high = 0.9,
    w = 1:150
  )
  marg <- marginal(
    fit,
    v = v,
    data = iris,
    y = iris$Sepal.Length,
    winsorize_low = 0.1,
    winsorize_high = 0.9,
    calc_pred = FALSE,
    pd_n = 0,
    w = 1:150
  )
  expect_equal(avg_obs, marg)
})
