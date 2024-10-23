
test_that("calculate_wdensity() matches published values", {
  # source: https://www.usgs.gov/special-topics/water-science-school/science/water-density:
  data_usgs <- data.frame(
  temperature_c = c(0, 4, 4.4, 10, 15.6, 21, 26.7, 32.2, 37.8),
     density_g_cm3 = c(0.99987,1,0.99999,0.99975,
                       0.99907,0.99802,0.99669,0.9951,0.99318)
   )
  # source: https://www.engineeringtoolbox.com/water-density-specific-weight-d_595.html
  data_eng_tbox <- data.frame(
  temperature_c = c(0.1, 1, 4, 10, 15, 20, 25, 30, 35),
     density_g_cm3 = c(0.9998495,0.9999017,0.9999749,
                       0.9997,0.9991026,0.9982067,0.997047,0.9956488,
                       0.9940326)
   )
  expect_equal(calculate_wdensity(data_usgs$temperature_c), data_usgs$density_g_cm3, tolerance = 5e-4)
  expect_equal(calculate_wdensity(data_eng_tbox$temperature_c), data_eng_tbox$density_g_cm3, tolerance = 5e-4)
})
