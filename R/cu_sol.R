
#' Calculate equilibrium copper solubility on a grid of input values
#'
#' @param ph A vector of pH values.
#' @param dic A vector of dissolved inorganic carbon concentrations, in mg C/L.
#' @param chloride A vector of chloride concentrations, in  mg/L.
#' @param sulfate A vector of sulfate concentrations, in mg SO4/L.
#' @param phosphate A vector of orthophosphate concentrations, in mg P/L.
#' @param phase Equilibrium phase.
#' @param db The database used in equilibrium solubility computations. The default is `pbcusol:::cu2sol`
#'
#' @return A tibble with the input grid of pH and DIC values and the copper solubilty predictions.
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' cu_sol(ph = 7.5, dic = 5, phase = "Tenorite")
#' cu_sol(ph = 7.5, dic = 5, db = phreeqc::minteq.v4.dat, phase = "Tenorite")
cu_sol <- function(
  ph,
  dic,
  chloride = 0,
  sulfate = 0,
  phosphate = 0,
  phase,
  db = cu2sol
) {

  tidyphreeqc::phr_use_db(db)

  #rho <- calculate_wdensity(25)

  tidyphreeqc::phr_run(
    tidyphreeqc::phr_solution_list(
      pH = ph,
      Cu = c("1", phase, "0") %>% paste(collapse = " "),
      C = dic / chemr::mass("C"),
      Cl = chloride / chemr::mass("Cl"),
      S = sulfate / chemr::mass("SO4"),
      P = phosphate / chemr::mass("P"),
      units = "mmol/l"
    ),
    tidyphreeqc::phr_selected_output(pH = TRUE, totals = c("C", "Cu"))
  ) %>%
    tibble::as_tibble() %>%
    dplyr::transmute(
      .data$pH,
      dic_ppm = 1e3 * chemr::mass("C") * .data$`C(mol/kgw)`,
      cu_ppb = 1e6 * chemr::mass("Cu") * .data$`Cu(mol/kgw)`
    )
}
