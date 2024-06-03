

#' Calculate equilibrium solubility in the presence of humic acid
#'
#' @description Compute equilibrium solubility in the presence of humic acid, using the
#' Windermere Humic Acid Model (WHAM), due to Tipping and Hurley (see `phreeqc::Tipping_Hurley.dat`,
#'  \url{https://doi.org/10.1016/0016-7037(92)90158-F}, and
#'  \url{https://water.usgs.gov/water-resources/software/PHREEQC/documentation/phreeqc3-html/phreeqc3.htm}).
#'  This function works like `eq_sol_fixed()`.
#'
#' @param ph pH
#' @param dic Dissolved inorganic carbon, in mg C/L.
#' @param phosphate Orthophosphate, in mg P/L.
#' @param phase Equilibrium phase.
#' @param element An element to return the equilibrium concentration of.
#' @param mass_ha Mass of humic acid in grams dissolved organic carbon.
#' @param mu_is An initial guess for the ionic strength, used to estimate the specific surface
#' area of the humic acid molecules.
#' @param phase_quantity Moles of equilibrium phase initially present.
#' @param pe Set the pe of the solution.
#' @param eq_phase_components Additional equilibrium phase components, passed to
#' `tidyphreeqc::phr_input_section` as a list.
#' @param surface_components Additional surface components, passed to
#' `tidyphreeqc::phr_input_section` as a list.
#' @param new_phase Define phases not included in the database.
#' @param phase_out Add an equilibrium phase to the output. Default is the pseudophase "Fix_pH".
#' @param new_species Define solution species not included in the database.
#' @param output_components Additional output components, passed to
#' `tidyphreeqc::phr_input_section` as a list.
#' @param buffer Substance added or subtracted from the solution to yield the desired pH.
#' @param db The database to use for equilibrium solubility computations. The default is
#' `leadsol`
#' @param max_iter Maximum iterations allowed for convergence of ionic strength.
#' @param  print Choose whether to print the input file ("input"), the full output ("output"), or the selected output.
#' Default is the latter.
#' @param ... Arguments passed on to `tidyphreeqc::phr_input_section()` as solution phase
#' components. Concentrations should be expressed in mmol/kgw.
#'
#' @return A tibble with columns representing equilibrium phase, pH, dissolved inorganic carbon,
#' orthophosphate (as P), pe, ionic strength (mu), total concentration of element in solution, and moles of the equilibrium phase
#' dissolved.
#' @importFrom dplyr %>%
#' @importFrom rlang :=
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' eq_sol_wham(element = "Pb", ph = 7.5, dic = 50, phase = "Cerussite", Na = 10, mass_ha = 3.5e-3)
eq_sol_wham <- function(
  ph,
  dic,
  phosphate = 0,
  phase,
  mass_ha = 0,
  element,
  mu_is = .003,
  phase_quantity = 1,
  pe = 4,
  eq_phase_components = list(),
  new_phase = list(),
  phase_out = "Fix_pH",
  new_species = list(),
  surface_components = list(),
  output_components = list(),
  buffer = "NaOH",
  db = pbcu2sol,
  max_iter = 3,
  print = NULL,
  ...
) {

  if(!is.null(print)) if(!print %in% c("input", "output"))
    stop("Valid entries for print are NULL, 'input', or 'output'")

  output_components <- if(length(output_components) == 0) {
    list("-totals" = c("P", "C", element))
  } else output_components

  # data from WHAM:
  surface_master_species <- phreeqc::Tipping_Hurley.dat[3169:3176]
  surface_species <- phreeqc::Tipping_Hurley.dat[3177:3355]

  # charge on 4 nHA sites:
  nHA_4 <- 2.84 / 4 * mass_ha / 1e3 # (eq)

  # charge on 12 diprotic sites:
  diprotic <- 2.84 / 12 * mass_ha / 1e3

  # solution:

  solution_components <- list(...)

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
      "C(4)" = if(is.numeric(dic)) dic / chemr::mass("C") else dic,
      "P" = phosphate / chemr::mass("P"),
      "pe" = pe,
      "temp" = 25,
      "redox" = "pe",
      "units" = "mmol/kgw",
      "density" = 1,
      "-water" = 1
    ) %>% c(solution_components)
  )

  # phases:

  pH_def <- tidyphreeqc::phr_pH_fix_definition()
  pe_def <- tidyphreeqc::phr_pe_fix_definition()

  add_phase <- tidyphreeqc::phr_input_section(
    type = "PHASES",
    components = new_phase
  )

  eq_phase <- tidyphreeqc::phr_input_section(
    type = "EQUILIBRIUM_PHASES",
    number = 1,
    name = "Solid",
    components = list(
      "phase" = c(0, phase_quantity),
      "Fix_pH" = c(-ph, buffer, 1e6),
      "Fix_pe" = c(-pe, "O2", 1e6)
    ) %>%
      rlang::set_names(c(phase, "Fix_pH", "Fix_pe")) %>%
      c(eq_phase_components)
  )

  # output

  out <- tidyphreeqc::phr_input_section(
    type = "SELECTED_OUTPUT",
    number = 1,
    components = list(
      "-equilibrium_phases" = paste(names(eq_phase$components), collapse = " "),
      "-state" = "true",
      "-mu" = "true",
      "-pH" = "true",
      "-pe" = "true"
    ) %>% c(output_components)
  )

  # humic acid surface:

  # For Psi vs I (= ionic strength) dependence, adapt specific surface area in PHRC:
  # SS = 159300 - 220800/(I)^0.09 + 91260/(I)^0.18
  # Example: SS = 46514 m2/g for I = 0.003 mol/l

  pb_sol_wham_iter <- function(mu_guess, output_type = NULL) {

    ssa <- 159300 - 220800/ mu_guess ^ 0.09 + 91260 / mu_guess ^ 0.18

    surface <- tidyphreeqc::phr_input_section(
      type = "SURFACE",
      components = list(
        # from example 19 ("Modeling Cd +2 Sorption With Linear, Freundlich, and Langmuir Isotherms, and
        # With a Deterministic Distribution of Sorption Sites for Organic Matter, Clay Minerals, and Iron Oxyhydroxides")

        # 3.5 mg OC, 0.025 meq total charge, distributed over the sites:
        # charge on 4 nHA sites: -2.84 / 4 * 3.5e-3 / 1e3 (eq)
        "H_a" = c(nHA_4, ssa, mass_ha),
        "H_b" = nHA_4,
        "H_c" = nHA_4,
        "H_d" = nHA_4,
        # charge on 4 nHB sites: 0.5 * charge on nHA sites
        "H_e" = 0.5 * nHA_4,
        "H_f" = 0.5 * nHA_4,
        "H_g" = 0.5 * nHA_4,
        "H_h" = 0.5 * nHA_4,
        # charge on 12 diprotic sites: -2.84 / 12 * 3.5e-3 / 1e3
        "H_ab" = diprotic,
        "H_ad" = diprotic,
        "H_af" = diprotic,
        "H_ah" = diprotic,
        "H_bc" = diprotic,
        "H_be" = diprotic,
        "H_bg" = diprotic,
        "H_cd" = diprotic,
        "H_cf" = diprotic,
        "H_ch" = diprotic,
        "H_de" = diprotic,
        "H_dg" = diprotic,
        "-Donnan",
        "-equilibrate" = 1
      ) %>%
        c(surface_components)
    )

    surface <- if(mass_ha == 0) NULL else surface

    run <- tidyphreeqc::phr_input(
      surface_master_species, surface_species,
      pH_def, pe_def, add_phase, add_species, surface,
      soln, eq_phase, out, tidyphreeqc::phr_end()
    )

    tidyphreeqc::phr_use_db(db)

    if(is.null(output_type)) {
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
          !!paste0(stringr::str_to_lower(element), "_ppb") := 1e6 * .data[[paste0(element, "(mol/kgw)")]] * chemr::mass(element),
          !!paste0("mol_", phase) := -.data[[paste0("d_", phase)]],
          !!paste0("mol_", phase_out) := -.data[[paste0("d_", phase_out)]]
        )
    } else
      if(output_type == "input") run else
        if(output_type == "output") {
          # full output:
          tidyphreeqc::phr_run(run) %>%
            tidyphreeqc::phr_print_output()
        }
  }

  # an initial guess for mu is required to estimate the specific surface area of the humic molecules;
  # if it's wrong, repeat the calculation using the updated value for mu until mu converges
  # or max_iter is reached:

  next_iter <- pb_sol_wham_iter(mu_guess = mu_is)

  mu_old <- mu_is
  mu_new <- next_iter$mu

  print_is_null <- if(mu_new - mu_old < 1e-5 | mass_ha == 0) next_iter else{

    counter <- 1

    while(mu_new - mu_old > 1e-5 & counter <= max_iter) {

      counter <- counter + 1                              # add one to iteration counter
      next_iter <- pb_sol_wham_iter(mu_guess = mu_new)    # use updated mu to run calculation
      mu_old <- mu_new                                    # updated mu becomes old mu
      mu_new <- next_iter$mu                              # update mu

    }

    next_iter
  }

  if(is.null(print)) print_is_null else
    pb_sol_wham_iter(mu_guess = mu_new, output_type = print)

}

#' @describeIn eq_sol_wham Shorthand for `eq_sol_wham()` with `element = "Pb"`. For backwards compatibility.
#' @export
pb_sol_wham <- function(element = "Pb", ...) {
  eq_sol_wham(element = element, ...)
}

#' @describeIn eq_sol_wham Shorthand for `eq_sol_wham()` with `element = "Cu"`. For backwards compatibility.
#' @export
cu_sol_wham <- function(element = "Cu", ...) {
  eq_sol_wham(element = element, ...)
}
