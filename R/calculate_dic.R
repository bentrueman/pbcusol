
#' Calculate dissolved inorganic carbon
#'
#' @param ph pH at which to perform the calculation
#' @param alkalinity Alkalinity, in mg CaCO3/L
#'
#' @return
#' @export
#'
#' @examples
#' calculate_dic(7.5, 20)
calculate_dic <- function(ph, alkalinity) { # provided alkalinity in mg CaCO3 / L
  alk <- alkalinity / 50044.5 # convert mg CaCO3 / L to eq / L
  h <- 10 ^ (-ph)
  poh <- 14 - ph
  oh <- 10 ^ (-poh)
  k1 <- 10 ^ -6.35
  k2 <- 10 ^ -10.33
  12011 * (alk + h - oh) * (h ^ 2 + k1 * h + k1 * k2) / (k1 * h + 2 * k1 * k2) # dic in mg/L
}
