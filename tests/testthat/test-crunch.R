test_that("qF2() works for non-factors", {
  data <- list(
    character = c("B", NA, "A", "B"),
    logical = c(TRUE, NA, FALSE, FALSE),
    integer = c(3L, 3L, 1L, NA),
    double = c(3, 3, 1, NA)
  )

  for (drop_na in c(FALSE, TRUE)) {
    for (x in data) {
      if (drop_na) {
        x <- x[!is.na(x)]
      }
      g <- qF2(x)
      expect_equal(g$bin_mid, unique(x))
      expect_equal(levels(g$g), as.character(g$bin_mid))
      expect_equal(class(g$bin_mid), class(x))
    }
  }
})

test_that("qF2 works for factors", {
  for (ordered in c(FALSE, TRUE)) {
    for (empty_levels in c(FALSE, TRUE)) {
      for (drop_na in c(FALSE, TRUE)) {
        z <- c("B", if (!drop_na) NA, "A", "B")
        lvl <- if (empty_levels) c("A", "B", "C") else c("A", "B")
        x <- factor(z, ordered = ordered, levels = lvl)
        g <- qF2(x)
        expect_equal(g$bin_mid, sort(unique(x), na.last = TRUE))
        expect_equal(levels(g$g), as.character(g$bin_mid))
        expect_equal(attributes(g$bin_mid), attributes(x))
      }
    }
  }
})

test_that("factor_or_double() works in the continuous case", {
  x <- 1:10
  res <- factor_or_double(x, m = 4)
  expect_equal(res, x)
  expect_true(is.double(res))

  # m needs to be smaller than length(ix_sub)
  expect_error(factor_or_double(x, m = 4, ix_sub = 1:2))
})

test_that("factor_or_double() works in the discrete case", {
  data <- list(
    character = c("B", NA, "A", "B"),
    logical = c(TRUE, NA, FALSE, FALSE),
    integer = 1:5,
    double = c(3, 3, 1, NA)
  )

  for (x in data) {
    g <- factor_or_double(x)
    expect_true(is.list(g))
  }

  # Compare with the previous test_that() block
  x <- 1:10
  expect_true(is.list(factor_or_double(x, m = 12)))
})

test_that("grouped_stats() works", {
  x <- cbind(a = 1:6, b = 6:1)
  g <- c(2, 2, 1, 1, 1, 1)
  w1 <- rep(2, times = 6)
  w2 <- 1:6

  r <- grouped_stats(x, g = g)
  rownames(r) <- NULL
  expect_equal(
    r[, 1:4],
    cbind(N = c(4, 2), weight = c(4, 2), a_mean = c(4.5, 1.5), b_mean = c(2.5, 5.5))
  )

  # Grouped and weighted
  rw1 <- grouped_stats(x, g = g, w = w1)
  rownames(rw1) <- NULL
  expect_equal(r[, c(1, 3:4)], rw1[, c(1, 3:4)])

  rw2 <- grouped_stats(x, g = g, w = w2)
  g1 <- colSums(x[g == 1, ] * w2[g == 1]) / sum(w2[g == 1])
  g2 <- colSums(x[g == 2, ] * w2[g == 2]) / sum(w2[g == 2])
  expect_equal(unname(rw2[, 1:4]), unname(cbind(c(4, 2), c(18, 3), rbind(g1, g2))))
})

test_that("Test that grouped_stats() uses sort(funique) + NA as order", {
  f1 <- c("b", "c", "c", NA, "a", "b")
  ff <- list(
    fact = factor(f1, levels = c("c", "b", "a")),
    float = c(3, 3, 1, 2, NA, 2),
    int = c(3L, 3L, 1L, 2L, NA, 2L),
    logi = c(TRUE, FALSE, FALSE, FALSE, NA, TRUE),
    char = f1
  )
  for (f in ff) {
    out <- rownames(grouped_stats(cbind(s = 1:6), g = f))
    expect_equal(out, as.character(sort(collapse::funique(f), na.last = TRUE)))
  }
})

test_that("fbreaks() without outlier handling gives same breaks like hist()", {
  set.seed(1)
  x <- rnorm(1000)

  breaks <- list(5, -10:10, "Sturges")
  for (b in breaks) {
    expect_equal(
      fbreaks(x, b, outlier_iqr = 0),
      graphics::hist(x, b, plot = FALSE)$breaks
    )
  }
})

test_that("fbreaks() without outliers gives same breaks like hist()", {
  x <- rep(0:1, times = c(90, 10)) # IQR is 0
  expect_equal(
    fbreaks(x, breaks = 5, outlier_iqr = 1.5),
    graphics::hist(x, breaks = 5, plot = FALSE)$breaks
  )
})

test_that("fbreaks() with outlier handling gives same breaks like hist()", {
  set.seed(1)
  x <- rnorm(1000)
  q <- wins_iqr(x, m = 1.5, ix_sub = 1:100)
  xcapped <- pmin(pmax(x, q[1L]), q[2L])

  breaks <- list(5, -10:10, "Sturges")
  for (b in breaks) {
    expect_equal(
      fbreaks(x, b, outlier_iqr = 1.5, ix_sub = 1:100),
      graphics::hist(xcapped, b, plot = FALSE)$breaks
    )
  }
})

test_that("fbreaks() does not like unknown strings", {
  expect_error(fbreaks(1:10, breaks = "scott"))
})

test_that("fcut() catches problematic input", {
  expect_error(fcut("a", breaks = 1:2))
  expect_error(fcut(1:3, breaks = 2:1))
  expect_error(fcut(1:3, breaks = 1))
  expect_error(fcut(1:3, breaks = 1:2, labels = TRUE))
  expect_error(fcut(1:3, breaks = 1:3, labels = "A"))
})

test_that("fcut() works in single-bin mode", {
  breaks <- 1:2
  n <- 10

  for (has_na in c(FALSE, TRUE)) {
    x <- c(if (has_na) NA, 1:n)
    z <- c(if (has_na) NA, rep("A", n))

    for (explicit_na in c(FALSE, TRUE)) {
      for (labels in list(FALSE, "A")) {
        out <- fcut(x, breaks = breaks, labels = labels, explicit_na = explicit_na)
        if (!isFALSE(labels)) {
          if (!explicit_na) {
            xp <- factor(z)
          } else {
            xp <- factor(z, levels = c("A", if (has_na) NA), exclude = NULL)
            class(xp) <- c("factor", "na.included")
          }
        } else {
          xp <- rep(1L, n)
          if (has_na) {
            xp <- c(if (explicit_na) 2L else NA, xp)
          }
        }
        expect_equal(out, xp)
      }
    }
  }
})

test_that("fcut() works in unequal- and equal-length mode", {
  n <- 10
  lev <- c("A", "B")
  for (equal in c(FALSE, TRUE)) {
    breaks <- c(1, 2 + equal, 5)
    for (has_na in c(TRUE, FALSE)) {
      x <- c(if (has_na) NA, 1:n)
      for (right in c(TRUE, FALSE)) {
        # "pre-expected"
        z <- rep(1:2, times = c(1L + right + equal, n - 1L - right - equal))
        if (has_na) {
          z <- c(NA, z)
        }
        for (explicit_na in c(TRUE, FALSE)) {
          for (labels in list(FALSE, lev)) {
            out <- fcut(
              x,
              breaks = breaks,
              labels = labels,
              explicit_na = explicit_na,
              right = right
            )
            if (!isFALSE(labels)) {
              if (!explicit_na) {
                xp <- factor(z, levels = 1:2, labels = lev)
              } else {
                xp <- factor(
                  z,
                  levels = c(1:2, if (has_na) NA),
                  labels = c(lev, if (has_na) NA),
                  exclude = NULL
                )
                class(xp) <- c("factor", "na.included")
              }
            } else { # no labels, just integers
              xp <- z
              if (has_na && explicit_na) {
                xp[is.na(xp)] <- 3L
              }
            }
            expect_equal(out, xp)
          }
        }
      }
    }
  }
})
