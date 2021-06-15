

#' Calculate equilibrium lead solubility, approximating the method of Jurgens et al.
#'
#' @description Experimental implementation of the approach to lead solubility calculation outlined in Jurgens et al.
#' \url{https://doi.org/10.1021/acs.est.8b04475}.
#'
#' @param Pb Concentrations of Pb initially present in solution (mg/L).
#' @param ph pH
#' @param dic Dissolved inorganic carbon, in mg C/L.
#' @param phosphate Orthophosphate, in mg P/L.
#' @param phase Equilibrium phase.
#' @param element An element to return the equilibrium concentration of.
#' @param eq_phase_components Additional equilibrium phase components, passed to
#' `tidyphreeqc::phr_input_section` as a list.
#' @param surface_components Components of a surface assemblage, passed to
#' `tidyphreeqc::phr_input_section` as a list.
#' @param new_phase Define phases not included in the database.
#' @param phase_out Add an equilibrium phase to the output. Default is the pseudophase "Fix_pH".
#' @param new_species Define solution species not included in the database.
#' @param output_components Additional output components, passed to
#' `tidyphreeqc::phr_input_section` as a list.
#' @param buffer Substance added or subtracted from the solution to yield the desired pH.
#' @param db The database to use for equilibrium solubility computations. The default is
#' `pbcusol:::leadsol`
#' @param  print Choose whether to print the input file ("input"), the full output ("output"), or the selected output.
#' Default is the latter.
#' @param ... Arguments passed on to `tidyphreeqc::phr_input_section()` as solution phase
#' components. Concentrations should be expressed in mmol/kgw.
#'
#' @return A tibble with columns representing equilibrium phase, pH, dissolved inorganic carbon,
#' orthophosphate (as P), pe, ionic strength (mu), total concentration of chosen element in solution, and moles of the equilibrium phase
#' dissolved.
#' @importFrom dplyr %>%
#' @importFrom rlang :=
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' pb_sol_jurgens(ph = 8, dic = 5)
pb_sol_jurgens <- function(
  Pb = 1,
  ph,
  dic,
  phosphate = 0,
  phase,
  element = "Pb",
  eq_phase_components = list(),
  new_phase = list(),
  phase_out = "Fix_pH",
  new_species = list(),
  surface_components = list(),
  output_components = list(),
  buffer = "NaOH",
  db = phreeqc::Amm.dat,
  print = NULL,
  ...
) {

  if(!is.null(print)) if(!print %in% c("input", "output"))
    stop("Valid entries for print are NULL, 'input', or 'output'")

  output_components <- if(length(output_components) == 0) {
    list("-totals" = c("P", "C", element))
  } else output_components

  # solution:

  add_species <- tidyphreeqc::phr_input_section(
    type = "SOLUTION_SPECIES",
    components = new_species
  )

  solution_components <- list(...)

  soln <- tidyphreeqc::phr_input_section(
    type =  "SOLUTION",
    number = 1,
    name = "water",
    components = list(
      "Pb" = Pb / chemr::mass("Pb"),
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

  # phases

  pH_def <- tidyphreeqc::phr_pH_fix_definition()
  pe_def <- tidyphreeqc::phr_pe_fix_definition()

  add_phase <- tidyphreeqc::phr_input_section(
    type = "PHASES",
    components = new_phase
  )

  eq_phase <- tidyphreeqc::phr_input_section(
    type = "EQUILIBRIUM_PHASES",
    number = 1,
    components = list(
      "Cerussite" = c(0, 0),
      "Hydrocerussite" =  c(0, 0),
      "Litharge" = c(0, 0),
      "Plattnerite" =  c(0, 0),
      "Pyromorphite-Cl" =  c(0, 0),
      "Pyromorphite-F" =  c(0, 0),
      "Pyromorphite-OH" =  c(0, 0),
      "Fix_pH" = c(-ph, buffer, 1e6),
      "Fix_pe" = c(-4, "O2", 1e6)
    )  %>%
      c(eq_phase_components)
  )

  # add a surface:

  add_surface <- tidyphreeqc::phr_input_section(
    type = "SURFACE",
    components = surface_components
  )

  # solid solution:

  solid_soln <- tidyphreeqc::phr_input_section(
    type = "SOLID_SOLUTIONS",
    number = 1,
    components = list(
      "Apatites",
      "-comp" = "Hydroxylapatite 1e-12",
      "-comp" = "Pyromorphite-Cl 0",
      "-comp" = "Pyromorphite-F 0",
      "-comp" =  "Pyromorphite-OH 0",
      "Carbonate",
      "-comp1" =  "Cerussite 0",
      "-comp2" =  "Calcite 0",
      "-tempk" =  "298.15",
      "-Gugg_nondim" =  "2.94 0"
    )
  )

  # output:

  phases <- c("CO2(g)", "Calcite", "Cerussite", "Litharge", "Pyromorphite-Cl",
    "Plattnerite", "Pyromorphite-F", "Pyromorphite-OH",
    "Fluorapatite", "Hydroxylapatite")

  out <- tidyphreeqc::phr_input_section(
    type = "SELECTED_OUTPUT",
    number = 1,
    components = list(
      "-equilibrium_phases" = phases,
      "-saturation_indices" = phases,
      "-state" = "true",
      "-mu" = "true",
      "-pH" = "true",
      "-pe" = "true"
    ) %>% c(output_components)
  )

  run <- tidyphreeqc::phr_input(
      jurgens, pH_def, pe_def, add_phase, add_species, add_surface,
      soln, eq_phase, solid_soln, out, tidyphreeqc::phr_end()
    )

  tidyphreeqc::phr_use_db(db)

  if(is.null(print)) {
    tidyphreeqc::phr_run(run) %>%
      tibble::as_tibble() %>%
      dplyr::filter(.data$state == "react") %>%
      dplyr::transmute(
        pH = .data$pH,
        dic_ppm = 1e3 * .data$`C(mol/kgw)` * chemr::mass("C"),
        p_ppm = 1e3 * .data$`P(mol/kgw)` * chemr::mass("P"),
        pe = .data$pe,
        mu = .data$mu,
        !!paste0(stringr::str_to_lower(element), "_ppb") := 1e6 * .data[[paste0(element, "(mol/kgw)")]] * chemr::mass(element),
        #!!paste0("mol_", phase) := -.data[[paste0("d_", phase)]],
        #!!paste0("mol_", phase_out) := -.data[[paste0("d_", phase_out)]]
      )
  } else
    if(print == "input") run else
      if(print == "output") {
        # full output:
        tidyphreeqc::phr_run(run) %>%
          tidyphreeqc::phr_print_output()
      }

}

