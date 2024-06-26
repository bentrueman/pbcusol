
#' Return a basic set of phases, reactions, and log K values involving Cu from a database.
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
#' cu_logk()
cu_logk <- function(kable_format = FALSE, db = pbcu2sol) {

  name <- type <- data <- NULL

  database <- db %>%
    tibble::enframe(name = NULL) %>%
    tibble::rowid_to_column()

  phases <- database %>%  # phases start on line 3437, solution species are before
    dplyr::filter(stringr::str_detect(.data$value, "PHASES")) %>%
    dplyr::pull(.data$rowid) %>%
    min()

  solution_species <- database %>%
    dplyr::filter(stringr::str_detect(.data$value, "SOLUTION_SPECIES")) %>%
    dplyr::pull(.data$rowid) %>%
    min()

  filter_these <- c("Hfo", "ine", "ate", "ide", "ta", "Al", "Se", "U", "As", "B", "Br",
    "F", "I", "V", "HS", "Cr", "Sb", "Pb", "Si") %>%
    paste(collapse = "|")

  database_wide <- database %>%
    dplyr::filter(
      .data$rowid > solution_species,
      stringr::str_detect(.data$value, "Cu") | # retain equations with Cu
        # and phase names (above equations):
        (.data$rowid > phases & stringr::str_detect(dplyr::lead(.data$value, 1), "Cu")) |
        stringr::str_detect(dplyr::lag(.data$value, 1), "Cu")  # and log K values (below equations)
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
    dplyr::filter(!stringr::str_detect(.data$eqn, filter_these)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      dplyr::across(tidyselect::everything(), \(x) stringr::str_remove_all(x, "\t")),
      name = dplyr::if_else(stringr::str_detect(name, "="), "", name),
      log_k = stringr::str_remove_all(.data$log_k, "log_k") %>%
        as.numeric(),
      eqn = stringr::str_trim(.data$eqn, side = "both")
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
        name = stringr::str_replace_all(name, "(\\))(\\d)", "\\1~\\2~") %>%
          stringr::str_replace_all("([:alpha:])(\\d)", "\\1~\\2~") %>%
          stringr::str_replace("CuMetal", "Cu metal")
      )
  } else table
}
