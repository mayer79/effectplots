fit <- lm(Sepal.Length ~ ., data = iris)
xvars <- colnames(iris)[-1]
M <- feature_effects(fit, v = xvars, data = iris, y = "Sepal.Length", breaks = 5)

test_that("subsetting and concatenating works", {
  # The test uses the fact that for smallish data, no seeding is needed
  M1 <- feature_effects(
    fit, v = xvars[1:2], data = iris, y = "Sepal.Length", breaks = 5
  )
  M2 <- feature_effects(
    fit, v = xvars[3:4], data = iris, y = "Sepal.Length", breaks = 5
  )

  expect_equal(M, c(M1, M2))
  expect_equal(M[3:4], M2)
})

test_that("print() gives no error", {
  capture_output(expect_no_error(print(M)))
  capture_output(expect_no_error(print(M[3])))
})

test_that("object is of class 'feature_effects'", {
  expect_s3_class(M, "EffectData")
})

