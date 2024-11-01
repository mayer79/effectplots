test_that("ale() is consistent with marginal()", {
  fit <- lm(Sepal.Length ~ ., data = iris)
  v <- c("Sepal.Width", "Species")
  suppressMessages(
    ale <- ale(fit, v = v, data = iris, w = 1:150)
  )
  suppressMessages(
    marg <- marginal(fit, v = v, data = iris, calc_pred = FALSE, pd_n = 0, w = 1:150)
  )
  expect_equal(ale, marg)
})
