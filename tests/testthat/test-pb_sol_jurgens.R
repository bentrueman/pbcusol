test_that("pb_sol_jurgens() returns expected output", {
  out <- tibble::tribble(
               ~pH, ~pe,                  ~mu,         ~dic_ppm,               ~p_ppm,              ~ca_ppm,          ~pb_ppb,
  8.00000003195453,   4, 0.000207174390600511, 5.00000000000022, 1.85842571991002e-08, 4.00784000000004e-08, 1.00000000002098
  )
  expect_equal(out, pb_sol_jurgens(ph = 8, dic = 5))
})
