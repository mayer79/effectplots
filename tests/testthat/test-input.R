test_that("prep_vec() does not accept anything", {
  expect_error(prep_vec(LETTERS))
  expect_error(prep_vec(c(1:3, NA, 3)))
})

test_that("prep_vec() returns what it should", {
  expect_equal(prep_vec(1:10), 1:10)
  expect_true(is.double(prep_vec(1:10)))

  expect_equal(prep_vec(c(TRUE, FALSE)), c(1, 0))
  expect_true(is.double(prep_vec(c(TRUE, FALSE))))
})

prep_pred <- function(x) {
  p <- NCOL(x)
  if (is.data.frame(x)) {
    x <- x[[p]]
  } else if (p > 1L) {
    x <- x[, p]
  } else if (!is.vector(x)) {
    x <- as.vector(x)
  }
  prep_vec(x)
}

test_that("prep_pred() returns what it should", {
  expect_equal(prep_pred(1:10), 1:10)
  expect_equal(prep_pred(c(TRUE, FALSE)), c(1, 0))

  expect_equal(prep_pred(data.frame(a = 1:10)), 1:10)
  expect_equal(prep_pred(data.frame(a = 1:10, b = 10:1)), 10:1)

  expect_equal(prep_pred(cbind(a = 1:10)), 1:10)
  expect_equal(prep_pred(cbind(a = 1:10, b = 10:1)), 10:1)
})

test_that("name_or_vector() does not accept anything", {
  expect_error(name_or_vector("s", data = iris))
  expect_error(name_or_vector(1:10, data = iris))
})

test_that("name_or_vector() returns what it should", {
  expect_equal(name_or_vector("Sepal.Width", data = iris), iris$Sepal.Width)
  expect_equal(name_or_vector(1:150, data = iris), 1:150)
})
