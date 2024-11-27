test_that("factor_or_double() works", {
  # Character
  x <- c("B", NA, "A")
  expect_equal(factor_or_double(x), collapse::qF(x, sort = FALSE, na.exclude = FALSE))

  # Factor
  x <- factor(x, levels = c("A", "B", "C"))
  expect_equal(
    factor_or_double(x), collapse::qF(x, sort = TRUE, na.exclude = FALSE, drop = TRUE)
  )

  # Logical
  x <- c(TRUE, FALSE)
  expect_equal(factor_or_double(x), collapse::qF(x, sort = FALSE, na.exclude = FALSE))

  # Non-discrete numeric vector (short)
  x <- 1:10
  res <- factor_or_double(x, m = 4)
  expect_equal(res, x)
  expect_true(is.double(res))

  # Discrete numeric vector (short)
  x <- rep(1:4, each = 10)
  res <- factor_or_double(x, m = 4)
  expect_equal(res, collapse::qF(x, na.exclude = FALSE, sort = FALSE))

  # Non-discrete numeric vector (long)
  x <- 1:10
  res <- factor_or_double(x, m = 4, ix_sub = 1:5)
  expect_equal(res, x)

  # Discrete numeric vector (long)
  x <- rep(1:4, each = 10)
  res <- factor_or_double(x, m = 4, ix_sub = 1:5)
  expect_equal(res, collapse::qF(x, na.exclude = FALSE, sort = FALSE))

  # m needs to be smaller than length(ix_sub)
  expect_error(factor_or_double(x, m = 4, ix_sub = 1:2))
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
  x <- c(NA, 1:10)

  # Codes only
  out <- fcut(x, breaks = breaks, labels = FALSE)
  expect_type(out, "integer")
  expect_equal(out, c(NA, rep(1, times = 10)))
  expect_equal(
    fcut(x, breaks = breaks, labels = FALSE, explicit_na = TRUE),
    c(2L, rep(1, times = 10))
  )

  # Factor output (without and with explicit missings)
  expect_equal(
    fcut(x, breaks = breaks, labels = "A"),
    as.factor(c(NA, rep("A", times = 10)))
  )
  xp <- factor(c(NA, rep("A", times = 10)), levels = c("A", NA), exclude = NULL)
  class(xp) <- c("factor", "na.included")
  expect_equal(fcut(x, breaks = breaks, labels = "A", explicit_na = TRUE), xp)

  # No missings, but explicit_na = TRUE; no labels
  x <- 1:10
  xp <- factor(rep("1", times = 10))
  class(xp) <- c("factor", "na.included")
  expect_equal(fcut(x, breaks = breaks, explicit_na = TRUE), xp)
})

test_that("fcut() works in unequal-length mode", {
  breaks <- c(1, 2, 5)
  x <- c(NA, 1:10)

  # Codes only: right = TRUE/FALSE/explicit.na = TRUE
  out <- fcut(x, breaks = breaks, labels = FALSE)
  expect_type(out, "integer")
  expect_equal(out, rep(c(NA, 1:2), times = c(1, 2, 8)))

  expect_equal(
    fcut(x, breaks = breaks, labels = FALSE, right = FALSE),
    rep(c(NA, 1:2), times = c(1, 1, 9))
  )

  expect_equal(
    fcut(x, breaks = breaks, labels = FALSE, explicit_na = TRUE),
    rep(c(3, 1:2), times = c(1, 2, 8))
  )

  # Factor output (without and with explicit missings)
  expect_equal(
    fcut(x, breaks = breaks, labels = c("A", "B")),
    as.factor(rep(c(NA, "A", "B"), times = c(1, 2, 8)))
  )
  xp <- factor(
    rep(c(NA, "A", "B"), times = c(1, 1, 9)),
    levels = c("A", "B", NA),
    exclude = NULL
  )
  class(xp) <- c("factor", "na.included")
  expect_equal(
    fcut(x, breaks = breaks, labels = c("A", "B"), explicit_na = TRUE, right = FALSE),
    xp
  )

  # No missings, but explicit_na = TRUE; no labels
  x <- 1:10
  xp <- factor(rep(c("1", "2"), times = c(2, 8)))
  class(xp) <- c("factor", "na.included")
  expect_equal(fcut(x, breaks = breaks, explicit_na = TRUE), xp)
})

test_that("fcut() works in equal-length mode", {
  breaks <- c(1, 4, 7)
  x <- c(NA, 1:10)

  # Codes only: right = TRUE/FALSE/explicit.na = TRUE
  out <- fcut(x, breaks = breaks, labels = FALSE)
  expect_type(out, "integer")
  expect_equal(out, rep(c(NA, 1:2), times = c(1, 4, 6)))

  expect_equal(
    fcut(x, breaks = breaks, labels = FALSE, right = FALSE),
    rep(c(NA, 1:2), times = c(1, 3, 7))
  )

  expect_equal(
    fcut(x, breaks = breaks, labels = FALSE, explicit_na = TRUE),
    rep(c(3, 1:2), times = c(1, 4, 6))
  )

  # Factor output (without and with explicit missings)
  expect_equal(
    fcut(x, breaks = breaks, labels = c("A", "B")),
    as.factor(rep(c(NA, "A", "B"), times = c(1, 4, 6)))
  )
  xp <- factor(
    rep(c(NA, "A", "B"), times = c(1, 3, 7)),
    levels = c("A", "B", NA),
    exclude = NULL
  )
  class(xp) <- c("factor", "na.included")
  expect_equal(
    fcut(x, breaks = breaks, labels = c("A", "B"), explicit_na = TRUE, right = FALSE),
    xp
  )

  # No missings, but explicit_na = TRUE; no labels
  x <- 1:10
  xp <- factor(rep(c("1", "2"), times = c(4, 6)))
  class(xp) <- c("factor", "na.included")
  expect_equal(fcut(x, breaks = breaks, explicit_na = TRUE), xp)
})


