
#' Calculate equilibrium copper solubility on a grid of input values
#'
#' @param ph A vector of pH values.
#' @param dic A vector of dissolved inorganic carbon concentrations, in mg C/L.
#' @param chloride A vector of chloride concentrations, in  mg/L.
#' @param sulfate A vector of sulfate concentrations, in mg SO4/L.
#' @param phosphate A vector of orthophosphate concentrations, in mg P/L.
#' @param phase Equilibrium phase.
#' @param db The database used in equilibrium solubility computations. The default is `pbcusol:::cu2sol`
#' @param empirical Logical. Predict equilibrium copper solubility using the empirical model due to Lytle et al.?
#' (https://doi.org/10.1002/awwa.1109)
#'
#' @return A tibble with the input grid of pH and DIC values and the copper solubilty predictions.
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' cu_sol(ph = 7.5, dic = 5, phase = "Tenorite")
#' cu_sol(ph = 7.5, dic = 5, db = phreeqc::minteq.v4.dat, phase = "Tenorite")
#' cu_sol(7.5, 5, phosphate = 1, empirical = TRUE)
cu_sol <- function(
  ph,
  dic,
  chloride = 0,
  sulfate = 0,
  phosphate = 0,
  phase,
  db = cu2sol,
  empirical = FALSE
) {

  if(empirical) {

    tidyr::crossing(pH = ph, dic_ppm = dic, p = phosphate) %>%
      dplyr::mutate(
        cu_ppb = 1e3 * 56.68 * exp(-.77 * .data$pH) *
          exp(-.2 * 3.066136 * .data$p) * .data$dic_ppm ^ .59
      )

  } else {

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
      tidyphreeqc::phr_selected_output(pH = TRUE, totals = c("C", "P", "Cu"))
      ) %>%
      tibble::as_tibble() %>%
      dplyr::transmute(
        .data$pH,
        dic_ppm = 1e3 * chemr::mass("C") * .data$`C(mol/kgw)`,
        p_ppm = 1e3 * chemr::mass("P") * .data$`P(mol/kgw)`,
        cu_ppb = 1e6 * chemr::mass("Cu") * .data$`Cu(mol/kgw)`
      )
  }

}
