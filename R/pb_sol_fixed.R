
#' Calculate equilibrium solubility with fixed pH
#'
#' @description While most lead phases are not soluble enough to have a substantial effect on
#' pH (or pe) as they dissolve, simulating extreme conditions may require fixing the pH. This is
#' accomplished here using `tidyphreeqc::phr_pH_fix_definition()`, with help from
#' \url{https://swilke-geoscience.net/post/phreeqc-mineral-solubility/}.
#'
#' @param ph pH
#' @param dic Dissolved inorganic carbon, in mg C/L.
#' @param phosphate Orthophosphate, in mg P/L.
#' @param phase Equilibrium phase.
#' @param eq_phase_components Additional equilibrium phase components, passed to
#' `tidyphreeqc::phr_input_section` as a list.
#' @param new_phase Define phases not included in the database.
#' @param new_species Define solution species not included in the database.
#' @param output_components Additional output components, passed to
#' `tidyphreeqc::phr_input_section` as a list.
#' @param buffer Substance added or subtracted from the solution to yield the desired pH.
#' @param db The database to use for equilibrium solubility computations. The default is
#' `pbcusol:::leadsol`
#' @param ... Arguments passed on to `tidyphreeqc::phr_input_section()` as solution phase
#' components. Concentrations should be expressed in mmol/kgw.
#'
#' @return A tibble with columns representing equilibrium phase, pH, dissolved inorganic carbon,
#' orthophosphate (as P), pe, ionic strength (mu), total lead, and moles of the equilibrium phase
#' dissolved.
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' pb_sol_fixed(ph = 6, dic = 5, phase = "Cerussite", buffer = "HCl")
pb_sol_fixed <- function(
  ph,
  dic,
  phosphate = 0,
  phase,
  eq_phase_components = list(),
  new_phase = list(),
  new_species = list(),
  output_components = list("-totals" = c("P", "C", "Pb")),
  buffer = "NaOH",
  db = pbcusol:::leadsol,
  ...
) {

  solution_components <- list(...)

  pH_def <- tidyphreeqc::phr_pH_fix_definition()
  pe_def <- tidyphreeqc::phr_pe_fix_definition()

  add_phase <- tidyphreeqc::phr_input_section(
    type = "PHASES",
    components = new_phase
  )

  add_species <- tidyphreeqc::phr_input_section(
    type = "SOLUTION_SPECIES",
    components = new_species
  )

  soln <- tidyphreeqc::phr_input_section(
    type =  "SOLUTION",
    number = 1,
    name = "water",
    components = list(
      "pH" = ph,
      "C(4)" = dic / chemr::mass("C"),
      "P" = phosphate / chemr::mass("P"),
      "pe" = 4,
      "temp" = 25,
      "redox" = "pe",
      "units" = "mmol/kgw",
      "density" = 1,
      "-water" = 1
    ) %>% c(solution_components)
  )

  eq_phase <- tidyphreeqc::phr_input_section(
    type = "EQUILIBRIUM_PHASES",
    number = 1,
    name = "Solid",
    components = list(
      "phase" = c(0, 10),
      "Fix_pH" = c(-ph, buffer, 1e6),
      "Fix_pe" = c(-4, "O2", 1e6)
    ) %>%
      rlang::set_names(c(phase, "Fix_pH", "Fix_pe")) %>%
      c(eq_phase_components)
  )

  out <- tidyphreeqc::phr_input_section(
    type = "SELECTED_OUTPUT",
    number = 1,
    components = list(
      "-equilibrium_phases" = phase,
      "-state" = "true",
      "-mu" = "true",
      "-pH" = "true",
      "-pe" = "true"
    ) %>% c(output_components)
  )

  run <- tidyphreeqc::phr_input(
    pH_def, pe_def, add_phase, add_species,
    soln, eq_phase, out, tidyphreeqc::phr_end()
  )

  tidyphreeqc::phr_use_db(db)

  d_phase <- paste0("d_", phase)

  tidyphreeqc::phr_run(run) %>%
    tibble::as_tibble() %>%
    dplyr::filter(.data$state == "react") %>%
    dplyr::transmute(
      phase,
      pH = .data$pH,
      dic_ppm = 1e3 * .data$`C(mol/kgw)` * chemr::mass("C"),
      p_ppm = 1e3 * .data$`P(mol/kgw)` * chemr::mass("P"),
      pe = .data$pe,
      mu = .data$mu,
      pb_ppb = 1e6 * .data$`Pb(mol/kgw)` * chemr::mass("Pb"),
      mol_dissolved = -.data[[paste0("d_", phase)]]
    )

}
