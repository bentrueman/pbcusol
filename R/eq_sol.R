

#' Calculate equilibrium solubility on a grid of input values
#'
#' @param element An element to return the equilibrium concentration of.
#' @param ph A vector of pH values.
#' @param dic A vector of dissolved inorganic carbon (DIC) concentrations, in mg C/L.
#' @param chloride A vector of chloride concentrations, in  mg/L.
#' @param sulfate A vector of sulfate concentrations, in mg SO4/L.
#' @param phosphate A vector of orthophosphate concentrations, in mg P/L.
#' @param phase Equilibrium phase.
#' @param db The database to use for equilibrium solubility computations. The default is `pbcusol:::pbcu2sol`
#' @param empirical Logical. Predict equilibrium copper solubility using the empirical model due to
#' Lytle et al.? \url{https://doi.org/10.1002/awwa.1109}?
#' @param ... Arguments passed on to `phr_solution_list()`
#'
#' @return A tibble with the input grid of pH and DIC values and the solubilty predictions.
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @importFrom rlang :=
#' @export
#'
#' @examples
#' eq_sol(element = "Pb", ph = 7.5, dic = 5, phase = "Cerussite")
#' eq_sol(element = "Pb", ph = 7.5, dic = 5, db = phreeqc::minteq.v4.dat, phase = "Cerussite")
#' eq_sol(element = "Cu", ph = 7.5, dic = 5, phase = "Tenorite")
#' eq_sol(element = "Cu", ph = 7.5, dic = 5, db = phreeqc::minteq.v4.dat, phase = "Tenorite")
#' eq_sol(element = "Cu", 7.5, 5, phosphate = 1, empirical = TRUE)
eq_sol <- function(
  ph,
  dic,
  chloride = 0,
  sulfate = 0,
  phosphate = 0,
  phase,
  element,
  db = pbcu2sol,
  empirical = FALSE,
  ...
) {

  if(empirical & element == "Cu") {

    tidyr::crossing(pH = ph, dic_ppm = dic, p = phosphate) %>%
      dplyr::mutate(
        cu_ppb = 1e3 * 56.68 * exp(-.77 * .data$pH) *
          exp(-.2 * 3.066136 * .data$p) * .data$dic_ppm ^ .59
      )

  } else {

  tidyphreeqc::phr_use_db(db)

  tidyphreeqc::phr_run(
    do.call(tidyphreeqc::phr_solution_list, rlang::list2(
      pH = ph,
      !!element := c("1", phase, "0") %>% paste(collapse = " "),
      C = dic / chemr::mass("C"),
      Cl = chloride / chemr::mass("Cl"),
      S = sulfate / chemr::mass("SO4"),
      P = phosphate / chemr::mass("P"),
      units = "mmol/l",
      ...
    )),
    tidyphreeqc::phr_selected_output(pH = TRUE, totals = c("C", "P", element))
  ) %>%
    tibble::as_tibble() %>%
    dplyr::transmute(
      .data$pH,
      dic_ppm = 1e3 * chemr::mass("C") * .data$`C(mol/kgw)`,
      p_ppm = 1e3 * chemr::mass("P") * .data$`P(mol/kgw)`,
      !!paste0(stringr::str_to_lower(element), "_ppb") := 1e6 * .data[[paste0(element, "(mol/kgw)")]] * chemr::mass(element),
    )
  }
}

#' @describeIn eq_sol Shorthand for `eq_sol()` with `element = "Pb"`. For backwards compatibility.
#' @export
pb_sol <- function(..., element = "Pb") {
  eq_sol(element = element, ...)
}

#' @describeIn eq_sol Shorthand for `eq_sol()` with `element = "Cu"`. For backwards compatibility.
#' @export
cu_sol <- function(..., element = "Cu") {
  eq_sol(element = element, ...)
}

