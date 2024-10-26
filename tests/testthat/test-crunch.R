test_that("winsorize() works", {
  x <- 1:10
  expect_equal(winsorize(x), x)
  expect_equal(winsorize(x, low = 2), pmax(2, x))
  expect_equal(winsorize(x, high = 8), pmin(8, x))  # not symmetric
  expect_equal(winsorize(x, low = 3, high = 7), pmax(3, pmin(7, x)))
})

test_that("rep_rows() gives the same as usual subsetting (except rownames)", {
  setrn <- function(x) {rownames(x) <- 1:nrow(x); x}

  expect_equal(rep_rows(iris, 1), iris[1, ])
  expect_equal(rep_rows(iris, 2:1), setrn(iris[2:1, ]))
  expect_equal(rep_rows(iris, c(1, 1, 1)), setrn(iris[c(1, 1, 1), ]))

  ir <- iris[1, ]
  ir$y <- list(list(a = 1, b = 2))
  expect_equal(rep_rows(ir, c(1, 1)), setrn(ir[c(1, 1), ]))
})

test_that("rep_rows() gives the same as usual subsetting for matrices", {
  ir <- data.matrix(iris[1:4])

  expect_equal(rep_rows(ir, c(1, 1, 2)), ir[c(1, 1, 2), ])
  expect_equal(rep_rows(ir, 1), ir[1, , drop = FALSE])
})

test_that("wrowmean() works", {
  x <- 6:1
  out <- wrowmean(x, ngroups = 2L)
  expect_equal(out, c(5, 2))
  expect_equal(wrowmean(x, ngroups = 3L), c(5.5, 3.5, 1.5))

  # Constant weights have no effect
  expect_equal(wrowmean(x, ngroups = 2L, w = c(1, 1, 1)), out)
  expect_equal(wrowmean(x, ngroups = 2L, w = c(4, 4, 4)), out)

  # Non-constant weights
  w <- 1:3
  a <- weighted.mean(6:4, w)
  b <- weighted.mean(3:1, w)
  out <- wrowmean(x, ngroups = 2L, w = w)
  expect_equal(out, c(a, b))
})

test_that("grouped_mean() works", {
  x <- cbind(a = 1:6, b = 6:1)
  g <- c(2, 2, 1, 1, 1, 1)
  w1 <- rep(2, times = 6)
  w2 <- 1:6

  r <- grouped_mean(x, g = g)
  rownames(r) <- NULL
  expect_equal(r, cbind(exposure = c(4, 2), a = c(4.5, 1.5), b = c(2.5, 5.5)))

  # Grouped and weighted
  rw1 <- grouped_mean(x, g = g, w = w1)
  rownames(rw1) <- NULL
  expect_equal(r[, 2:3], rw1[, 2:3])

  rw2 <- grouped_mean(x, g = g, w = w2)
  rownames(rw2) <- c("g1", "g2")
  g1 <- colSums(x[g == 1, ] * w2[g == 1]) / sum(w2[g == 1])
  g2 <- colSums(x[g == 2, ] * w2[g == 2]) / sum(w2[g == 2])
  expect_equal(rw2, cbind(exposure = c(18, 3), rbind(g1, g2)))
})

test_that("poor_man_stack() works (test could be improved)", {
  y <- c("a", "b", "c")
  z <- c("aa", "bb", "cc")
  X <- data.frame(x = 1:3, y = y, z = z)
  out <- poor_man_stack(X, to_stack = c("y", "z"))
  xpected <- data.frame(
    x = rep(1:3, times = 2L),
    varying_ = factor(rep(c("y", "z"), each = 3L)),
    value_ = c(y, z)
  )
  expect_equal(out, xpected)

  expect_error(poor_man_stack(cbind(a = 1:3, b = 2:4), to_stack = "b"))
})

test_that("Test that grouped_mean() uses sort(unique) + NA as order", {
  f <- c("b", "c", "c", NA, "a", "b")
  ff <- list(
    fact = factor(f, levels = c("c", "b", "a")),
    float = c(3, 3, 1, 2, NA, 2),
    int = c(3L, 3L, 1L, 2L, NA, 2L),
    logi = c(TRUE, FALSE, FALSE, FALSE, NA, TRUE),
    char = f
  )
  for (f in ff) {
    out <- rownames(grouped_mean(cbind(1:6), g = f))
    expect_equal(out, as.character(sort(unique(f), na.last = TRUE)))
  }
})


