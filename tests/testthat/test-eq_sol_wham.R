test_that("eq_sol_wham() matches eq_sol_fixed() when mass_ha == 0", {
  pb1 <- eq_sol_fixed(8, 50, 0,  "Cerussite", element = "Pb")
  pb2 <- eq_sol_wham(8, 50, 0,  "Cerussite", element = "Pb")
  expect_equal(pb1, pb2)
})
