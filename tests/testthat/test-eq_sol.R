test_that("Hydrocerussite solubility minimum is ~ 68 ppb", {
  expect_equal(round(eq_sol_fixed(9.8, 3.7, 0,  "Hydcerussite", element = "Pb")$pb_ppb), 68)
})
