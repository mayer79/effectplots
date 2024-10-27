test_that("partial_dependence() is consistent with marginal()", {
  fit <- lm(Sepal.Length ~ ., data = iris)
  v <- c("Sepal.Width", "Species")
  pd <- partial_dependence(fit, v = v, data = iris, w = 1:150)
  marg <- marginal(fit, v = v, data = iris, calc_pred = FALSE, w = 1:150)
  expect_equal(pd, marg)
})
