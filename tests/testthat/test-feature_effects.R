test_that("setting a seed works", {
  X <- data.frame(matrix(runif(1e5), ncol = 4))
  pf <- function(m, x) rowMeans(x)
  old_seed <- .Random.seed
  M <- feature_effects("anymodel", v = colnames(X), data = X, pred_fun = pf, seed = 7)
  new_seed <- .Random.seed
  expect_equal(old_seed, new_seed)

  M <- feature_effects("anymodel", v = colnames(X), data = X, pred_fun = pf)
  expect_false(identical(new_seed, .Random.seed))
})
