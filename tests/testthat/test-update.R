set.seed(1)

fit <- lm(Sepal.Length ~ Species + Petal.Width, data = iris, weights = Sepal.Width)
M <- feature_effects(
  fit,
  v = c("Petal.Length", "Petal.Width", "Species"),
  data = iris[c(11:50, 61:100, 101:150), ],
  y = "Sepal.Length",
  w = "Sepal.Width",
  breaks = 5
)

test_that("update() can drop small levels", {
  # N
  res <- update(M, drop_below_n = 9)
  expect_true(all(res$Petal.Width$N >= 9))

  res <- update(M["Species"], drop_below_n = 45)
  expect_equal(nrow(res$Species), 1L)

  # Weight
  res <- update(M, drop_below_weight = 20)
  expect_true(all(res$Petal.Width$weight >= 20))

  res <- update(M["Species"], drop_below_weight = 140)
  expect_equal(nrow(res$Species), 1L)
})

test_that("update() can collapse levels of a factor", {
  res <- update(M, collapse_m = 2)

  # No effect on numeric features
  expect_equal(M$Petal.Width, res$Petal.Width)

  # The result has the right levels (we kept all rows of virginica)
  expect_equal(levels(res$Species$bin_mid), c("virginica", "other"))

  # The virginica row remained the same
  expect_equal(
    droplevels(M$Species[M$Species$bin_mid == "virginica", ]),
    droplevels(res$Species[res$Species$bin_mid == "virginica", ])
  )

  # The other two rows have been collapsed
  s1 <- M$Species$bin_mid != "virginica"
  s2 <- res$Species$bin_mid == "other"
  expect_equal(sum(M$Species$N[s1]), sum(res$Species$N[s2]))
  expect_equal(sum(M$Species$weight[s1]), sum(res$Species$weight[s2]))
  expect_equal(
    weighted.mean(M$Species$pd[s1], M$Species$weight[s1]),
    res$Species$pd[s2]
  )
  expect_equal(
    weighted.mean(M$Species$y_sd[s1]^2, M$Species$weight[s1]),
    res$Species$y_sd[s2]^2
  )
})

test_that("update() can collapse levels of a character", {
  ir <- transform(iris, Species = as.character(Species))
  fit2 <- lm(Sepal.Length ~ Species + Petal.Width, data = ir)
  M2 <- feature_effects(
    fit2,
    v = c("Petal.Length", "Petal.Width", "Species"),
    data = ir[c(11:50, 61:100, 101:150), ],
    y = "Sepal.Length",
    breaks = 5
  )

  res <- update(M2, collapse_m = 2)

  # No effect on numeric features
  expect_equal(M2$Petal.Width, res$Petal.Width)

  # The result has the right levels (we kept all rows of virginica)
  expect_equal(res$Species$bin_mid, c("virginica", "other"))
})

test_that("update() can remove NA levels", {
  M2 <- M
  M2$Petal.Width$bin_mid[1] <- NA
  expect_true(!anyNA(update(M2, na.rm = TRUE)$Petal.Width))
})

test_that("update() can sort according to importance", {
  imp <- effect_importance(M, "pd")

  # Petal.Length was not used as covariate -> pd importance of 0
  expect_equal(names(imp[imp < 1e-6]), "Petal.Length")

  # Update reorders M according to importance
  expect_equal(names(update(M, sort = "pd")), names(sort(-imp)))

  # Since categoricals do not have ALE value, their importance should be 0 as well
  imp <- effect_importance(M, "ale")
  expect_equal(names(imp[imp > 1e-4]), "Petal.Width")
  expect_equal(names(update(M, sort = "ale"))[1], "Petal.Width")
})

test_that("update() can drop empty levels", {
  M2 <- feature_effects(
    fit,
    v = c("Petal.Length", "Petal.Width", "Species"),
    data = iris[51:150, ],
    y = "Sepal.Length",
    w = "Sepal.Width",
    breaks = 5
  )

  # setosa is here and stays with a normal update
  expect_equal(M2$Species[1L, "N"], 0)
  expect_equal(update(M2)$Species[1L, "N"], 0)

  # Not anymore
  expect_false("setosa" %in% levels(update(M2, drop_empty = TRUE)$Species$bin_mid))
})

