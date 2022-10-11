
#' Return  a basic set of phases, reactions, and log K values involving Pb from a database
#'
#' @param kable_format Logical. Format the table for `knitr::kable`?
#' @param db The database from which to extract phases, reactions, and log K values. The default is the curated dataset
#' included in the package, but the function is also designed to work for phreeqc::minteq.dat and phreeqc::minteq.v4.dat.
#'
#' @return A tibble with phase names, reactions, and log K values.
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' pb_logk()
pb_logk <- function(kable_format = FALSE, db = pbcusol:::pbcu2sol) {

  name <- type <- data <- NULL

  database <- db %>%
    tibble::enframe(name = NULL) %>%
    tibble::rowid_to_column()

  phases <- database %>%  # in LEADSOL, phases start on line 3401, solution species are before
    dplyr::filter(stringr::str_detect(.data$value, "PHASES")) %>%
    dplyr::pull(.data$rowid) %>%
    min()

  solution_species <- database %>%
    dplyr::filter(stringr::str_detect(.data$value, "SOLUTION_SPECIES")) %>%
    dplyr::pull(.data$rowid) %>%
    min()

  remove_these <- c("ine", "ate", "ide", "ta", "Al", "Se", "U", "As", "B",
    "Br", "F", "I", "V", "HS", "N", "Cr", "Cu", "Metal") %>%  # these will be filtered out
    paste(collapse = "|")

  keep_these <- paste(c("[cC]erussite", "pyromorphite", "="), collapse = "|")

  database_wide <- database %>%
    dplyr::filter(
      .data$rowid > solution_species,# exclude solution master species
      stringr::str_detect(.data$value, "Pb") | # retain equations with Pb
        # and phase names (above equations):
        (.data$rowid > phases & stringr::str_detect(dplyr::lead(.data$value, 1), "Pb")) |
        stringr::str_detect(dplyr::lag(.data$value, 1), "Pb")
    ) %>%
    dplyr::mutate(
      type = dplyr::case_when( # indicates type of data in value column
        stringr::str_detect(.data$value, "=") ~ "eqn",
        stringr::str_detect(.data$value, "log_k") ~ "log_k",
        TRUE ~ "phase"
      ),
      name = dplyr::case_when( # reaction (solution species) or name of phase
        .data$rowid < phases & .data$type == "eqn" ~ .data$value,
        .data$rowid >= phases & .data$type == "phase" ~ .data$value
      )
    ) %>%
    tidyr::fill(name) %>%
    dplyr::filter(.data$type != "phase") %>% # remove b/c phase transferred to name column
    dplyr::group_by(name, .data$type) %>%
    # collapses duplicate name and type combinations to same row
    dplyr::summarize(data = paste(.data$value, collapse = ", ")) %>%
    dplyr::select(name, type, data) %>%
    tidyr::spread(key = .data$type, value = .data$data)

  table <- database_wide %>%
    dplyr::filter(
      !stringr::str_detect(.data$eqn, remove_these),
      stringr::str_detect(name, keep_these)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      dplyr::across(tidyselect::everything(), \(x) stringr::str_remove_all(x, "\t")),
      # remove all but phase names from name column
      name = dplyr::if_else(stringr::str_detect(name, "ite"), name, ""),
      log_k = stringr::str_remove(.data$log_k, "log_k") %>%
        as.numeric()
    ) %>%
    dplyr::arrange(name, stringr::str_remove_all(.data$eqn, "[0-9]"))

  if(kable_format) {
    table %>%
      dplyr::mutate(
        eqn = stringr::str_replace_all(.data$eqn, "(\\+\\d)", "^\\1^") %>% # superscript charges
          stringr::str_replace_all("(\\-\\d)", "^\\1^") %>% # superscript charges
          stringr::str_replace_all("(\\w)(\\+)", "\\1^\\2^") %>% # superscript charges
          stringr::str_replace_all("(\\w)(\\-)", "\\1^\\2^") %>% # superscript charges
          stringr::str_replace_all("(\\))(\\d)", "\\1~\\2~") %>% # subscripts in formulas
          stringr::str_replace_all("(\\w)(\\d)", "\\1~\\2~"), # subscripts in formulas
        name = dplyr::case_when(
          name == "Hxypyromorphite" ~ "Hydroxylpyromorphite",
          name == "Hydcerussite" ~ "Hydrocerussite",
          TRUE ~ name
        )
    )
  } else table
}
