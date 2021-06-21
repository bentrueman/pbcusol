test_that("DIC calculation works", {
  expect_equal(round(calculate_dic(7.5, 20)), 5)
})
