
#' Calculate equilibrium lead solubility on a grid of input values
#'
#' @param ph A vector of pH values.
#' @param dic A vector of dissolved inorganic carbon (DIC) concentrations, in mg C/L.
#' @param chloride A vector of chloride concentrations, in  mg/L.
#' @param sulfate A vector of sulfate concentrations, in mg SO4/L.
#' @param phosphate A vector of orthophosphate concentrations, in mg P/L.
#' @param phase Equilibrium phase.
#' @param db The database to use for equilibrium solubility computations. The default is `pbcusol:::leadsol`
#'
#' @return A tibble with the input grid of pH and DIC values and the lead solubilty predictions.
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' pb_sol(ph = 7.5, dic = 5, phase = "Cerussite")
#' pb_sol(ph = 7.5, dic = 5, db = phreeqc::minteq.v4.dat, phase = "Cerussite")
pb_sol <- function(
  ph,
  dic,
  chloride = 0,
  sulfate = 0,
  phosphate = 0,
  phase,
  db = leadsol
) {

  tidyphreeqc::phr_use_db(db)

  #rho <- calculate_wdensity(25)

  tidyphreeqc::phr_run(
    tidyphreeqc::phr_solution_list(
      pH = ph,
      Pb = c("1", phase, "0") %>% paste(collapse = " "),
      C = dic / chemr::mass("C"),
      Cl = chloride / chemr::mass("Cl"),
      S = sulfate / chemr::mass("SO4"),
      P = phosphate / chemr::mass("P"),
      units = "mmol/l"
    ),
    tidyphreeqc::phr_selected_output(pH = TRUE, totals = c("C", "Pb"))
  ) %>%
    tibble::as_tibble() %>%
    dplyr::transmute(
      .data$pH,
      dic_ppm = 1e3 * chemr::mass("C") * .data$`C(mol/kgw)`,
      pb_ppb = 1e6 * chemr::mass("Pb") * .data$`Pb(mol/kgw)`
    )
}
