test_that("calculate_dic() works", {
  expect_equal(calculate_dic(7.5, 20), 5.128215)
  expect_equal(calculate_dic(7.5, 20, method = "b"), 5.1238897)
})

test_that("calculate_dic() returns an error if method is not a or b", {
  expect_error(calculate_dic(7.5, 20, method = "c"))
})

test_that("calculate_dic() returns NA when alkalinity is NA", {
  skip_on_ci() # runs locally but not via GHA
  expect_equal(
    calculate_dic(c(9, 8), c(NA, 19), c(10, 20), method = "b"), c(NA, 4.63918), tolerance = 1e-5
  )
  expect_equal(
    calculate_dic(c(9, 10), c(NA, 19), c(10, 20), method = "a"), c(NA, 2.54765), tolerance = 1e-5
  )
})

test_that("calculate_dic() returns NA when pH is NA (method a)", {
  expect_equal(
    calculate_dic(c(NA, 10), c(12, 19), c(10, 20), method = "a"), c(NA, 2.54765), tolerance = 1e-5
  )
})

test_that("calculate_dic() returns an error when pH or temperature are NA (method b)", {
  expect_error(calculate_dic(c(NA, 10), c(12, 19), c(10, 20), method = "b"))
  expect_error(calculate_dic(c(9, 10), c(12, 19), c(NA, 20), method = "b"))
})

