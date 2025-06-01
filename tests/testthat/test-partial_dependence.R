test_that("partial_dependence() is consistent with feature_effects()", {
  fit <- lm(Sepal.Length ~ ., data = iris)
  v <- c("Sepal.Width", "Species")
  pd <- partial_dependence(fit, v = v, data = iris, w = 1:150)
  marg <- feature_effects(
    fit,
    v = v, data = iris, calc_pred = FALSE, ale_n = 0, w = 1:150
  )
  expect_equal(pd, marg)
})


test_that(".pd() gives same answer on example as iml 0.11.1", {
  fit <- lm(Sepal.Length ~ . + Petal.Width:Species, data = iris)

  # library(iml)
  # mod <- Predictor$new(fit, data = iris)
  # FeatureEffect$new(mod, feature = "Species", method = "pdp")$results
  # FeatureEffect$new(mod, feature = "Sepal.Width", method = "pdp", grid.points = 2:4)$results

  iml_species <- c(6.847179, 5.737053, 5.260083)
  raw_species <- .pd(fit, v = "Species", data = iris, grid = unique(iris$Species))
  expect_equal(iml_species, raw_species, tolerance = 0.001)

  iml_sw <- c(5.309279, 5.814375, 6.319470)
  raw_sw <- c(.pd(fit, v = "Sepal.Width", data = iris, grid = 2:4))
  expect_equal(iml_sw, raw_sw, tolerance = 0.001)
})

test_that(".pd() works with case weights on non-trivial example", {
  fit <- lm(Sepal.Length ~ . + Petal.Width:Species, data = iris)

  w <- rep(2, times = 150)
  pd1 <- .pd(fit, v = "Species", data = iris, grid = unique(iris$Species))
  pd2 <- .pd(fit, v = "Species", data = iris, grid = unique(iris$Species), w = w)
  expect_equal(pd1, pd2)

  pd2 <- .pd(fit, v = "Species", data = iris, grid = unique(iris$Species), w = 1:150)
  expect_false(identical(pd1, pd2))
})

test_that(".pd() respects ... argument for predictions", {
  fit2 <- glm(Sepal.Length ~ . + Petal.Width:Species, data = iris, family = Gamma())

  pd1 <- .pd(fit2, v = "Species", data = iris, grid = unique(iris$Species))
  pd2 <- .pd(
    fit2,
    v = "Species", data = iris, grid = unique(iris$Species), type = "response"
  )
  expect_false(identical(pd1, pd2))
})

test_that(".pd() respects case weights", {
  fit <- lm(Sepal.Length ~ Species * Sepal.Width, data = iris)
  v <- "Sepal.Width"
  br <- c(2, 2.5, 3, 3.5, 4.5)
  w <- c(rep(1L, times = 100L), rep(2L, times = 50L))
  ix <- rep(1:nrow(iris), times = w)

  res_w <- .pd(fit, v = v, data = iris, grid = br, w = w)
  res_uw <- .pd(fit, v = v, data = iris[ix, ], grid = br)

  expect_equal(res_w, res_uw)
})

test_that("partial_dependence() respects case weights", {
  fit <- lm(Sepal.Length ~ Species * Sepal.Width, data = iris)
  v <- "Sepal.Width"
  br <- c(2, 2.5, 3, 3.5, 4.5)
  w <- c(rep(1L, times = 100L), rep(2L, times = 50L))
  ix <- rep(1:nrow(iris), times = w)

  res_w <- partial_dependence(fit, v = v, data = iris, breaks = br, w = w)[[1L]]
  res_uw <- partial_dependence(fit, v = v, data = iris[ix, ], breaks = br)[[1L]]

  expect_equal(res_w[-4L], res_uw[-4L])
})
