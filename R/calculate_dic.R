
#' Calculate dissolved inorganic carbon
#'
#' @param ph A vector of pH values.
#' @param alkalinity A vector of alkalinities, in mg CaCO3/L
#' @param temperature Water tempterature in degress C
#' @param method Either "a" or "b". Method "a", the default, is described in https://doi.org/10.1002/j.1551-8833.1980.tb04616.x, and it
#' applies at 25 degrees C only. Method "b" uses PHREEQC and incorporates temperature as a variable, but it may not converge.
#'
#' @return A numeric vector of dissolved inorganic carbon concentrations, in mg C/L.
#' @export
#'
#' @examples
#' calculate_dic(7.5, 20)
calculate_dic <- function(ph, alkalinity, temperature = 25, method = "a") {

  if (method %in% c("a", "b")) {
    if (method == "a") dic <- use_method_a(ph, alkalinity)
    if (method == "b") dic <- use_method_b(ph, alkalinity, temperature)
  } else
    stop("method must be either 'a' or 'b'")
  dic
}

use_method_a <- function(ph, alkalinity) {
  alk <- alkalinity / 50044.5 # convert mg CaCO3 / L to eq / L
  h <- 10 ^ (-ph)
  poh <- 14 - ph
  oh <- 10 ^ (-poh)
  k1 <- 10 ^ -6.35
  k2 <- 10 ^ -10.33
  12011 * (alk + h - oh) * (h ^ 2 + k1 * h + k1 * k2) / (k1 * h + 2 * k1 * k2) # dic in mg/L
}

use_method_b <- function(ph, alkalinity, temperature) {

  wq <- data.frame(
    ph,
    alkalinity = alkalinity / chemr::mass("Ca0.5(CO3)0.5"),
    temperature
  )

  dic <- numeric(length = nrow(wq))

  for(i in seq_len(nrow(wq))) {
    out <- tidyphreeqc::phr_run(
      tidyphreeqc::phr_solution_list(pH = wq$ph[i], temp = wq$temperature[i], Alkalinity = wq$alkalinity[i], units = "mmol/L"),
      tidyphreeqc::phr_selected_output(totals = "C")
    )
    dic[i] <- tibble::as_tibble(out)$`C(mol/kgw)` * chemr::mass("C") * 1e3
  }

  ifelse(is.na(alkalinity), NA, dic)

}
