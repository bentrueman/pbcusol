test_that("Hydrocerussite solubility minimum is ~ 68 ppb", {
  expect_equal(round(eq_sol_fixed(9.8, 3.7, 0,  "Hydcerussite", element = "Pb")$pb_ppb), 68)
})

test_that("eq_sol() accepts the argument 'print = '", {
  expect_equal(
    class(pb_sol(ph = 7, dic = 5, phase = "Cerussite", print = "input")),
    "phr_input"
  )
})
