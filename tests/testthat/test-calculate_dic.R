test_that("calculate_dic() works", {
  expect_equal(calculate_dic(7.5, 20), 5.128215)
  expect_equal(calculate_dic(7.5, 20, method = "b"), 5.1238897)
})

test_that("calculate_dic() returns an error if method is not a or b", {
  expect_error(calculate_dic(7.5, 20, method = "c"))
})
