test_that("calculate_wdensity() returns the expected result", {
  temps <- c(0, 10, 20, 30)
  expect_equal(
    round(calculate_wdensity(temps), 7),
    c(0.9998395, 0.9995457, 0.9979355, 0.9952905)
  )
})
