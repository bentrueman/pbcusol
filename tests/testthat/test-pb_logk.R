test_that("pb_logk() generates the expected output.", {
  expect_snapshot(pb_logk())
  expect_snapshot(pb_logk(kable_format = TRUE))
})
