test_that("partial_dependence() is consistent with feature_effects()", {
  fit <- lm(Sepal.Length ~ ., data = iris)
  v <- c("Sepal.Width", "Species")
  pd <- partial_dependence(fit, v = v, data = iris, w = 1:150)
  marg <- feature_effects(
    fit, v = v, data = iris, calc_pred = FALSE, ale_n = 0, w = 1:150
  )
  expect_equal(pd, marg)
})
