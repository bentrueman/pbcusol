
#' Calculate the density of air-free water at 0 - 150 C and 101.325 kPa
#'
#' @param temp A vector of water temperatures, in degrees celsius.
#'
#' @return A vector of densities, in kg/L.
#' @export
#'
#' @examples
#'calculate_wdensity(25)
calculate_wdensity <- function(temp) {

  # formula source: doi:10.6028/jres.097.013

  a <-  999.83952
  b <-  16.945176
  c <-  -7.987040e-3
  d <-  -46.170461e-6
  e <-  105.56302e-9
  f <-  -280.54253e-12
  g <-  16.897850e-3

  1e-3 * (a + b * temp + c * temp ^ 2 + d * temp ^ 3 + e * temp ^ 4 + f * temp ^ 5) /
    (1 + g * temp)

}
