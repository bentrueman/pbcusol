test_that("pb_logk() generates the expected output.", {
  expect_snapshot(cu_logk())
  expect_snapshot(cu_logk(kable_format = TRUE))
})
