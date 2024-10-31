test_that("prep_pred() returns what it should", {
  p <- prep_pred(1:10)
  expect_equal(p, 1:10)
  expect_true(is.double(p))

  p <- prep_pred(c(TRUE, FALSE))
  expect_equal(p, c(1, 0))
  expect_true(is.double(p))

  expect_equal(prep_pred(data.frame(a = 1:10)), 1:10)
  expect_equal(prep_pred(data.frame(a = 1:10, b = 10:1)), 10:1)

  expect_equal(prep_pred(cbind(a = 1:10)), 1:10)
  expect_equal(prep_pred(cbind(a = 1:10, b = 10:1)), 10:1)

  expect_error(prep_vec(LETTERS))
  expect_error(prep_vec(c(1:3, NA, 3)))
})

test_that("basic_check() works", {
  nms <- letters[1:3]
  n <- 10

  # NULL is ok
  expect_true(basic_check(NULL, n = n, nms = nms))

  # Vector of right length is ok
  expect_true(basic_check(1:10, n = n, nms = nms))
  expect_false(basic_check(1:8, n = n, nms = nms))
  expect_false(basic_check(cbind(1:10), n = n, nms = nms))

  # Variable name is ok
  expect_true(basic_check("a", n = n, nms = nms))
  expect_false(basic_check("a", n = n, nms = letters[4:6]))
})
