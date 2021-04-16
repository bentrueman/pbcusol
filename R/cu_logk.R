
#' Return CU2SOL phases, reactions, and log K values
#'
#' @param kable_format Logical. Format the table for `knitr::kable`?
#'
#' @return A tibble with phase names, reactions, and log K values.
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' cu_logk()
cu_logk <- function(kable_format = FALSE) {

  database <- cu2sol %>%
    tibble::enframe(name = NULL) %>%
    tibble::rowid_to_column()

  phases <- database %>%  # phases start on line 3437, solution species are before
    dplyr::filter(stringr::str_detect(.data$value, "PHASE")) %>%
    dplyr::pull(.data$rowid)


  filter_these <- c("ine", "ate", "ide", "ta", "Al", "Se", "U", "As", "B", "Br",
    "F", "I", "V", "HS", "Cr", "Sb", "Pb", "Si") %>%
    paste(collapse = "|")

  table <- database %>%
    dplyr::filter(
      .data$rowid > 152, # solution reactions start on line 152, lines 1 - 151are essential definitions
      stringr::str_detect(.data$value, "Cu") | # retain equations with Cu
        (.data$rowid > phases & dplyr::lead(.data$value, 1) %>% stringr::str_detect("Cu")) | # and phase names (above equations)
        dplyr::lag(.data$value, 1) %>% stringr::str_detect("Cu")  # and log K values (below equations)
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
    tidyr::fill(.data$name) %>%
    dplyr::filter(.data$type != "phase") %>% # remove b/c phase transferred to name column
    dplyr::group_by(.data$name, .data$type) %>%
    # collapses duplicate name and type combinations to same row
    dplyr::summarize(data = paste(.data$value, collapse = ", ")) %>%
    dplyr::select(.data$name, .data$type, .data$data) %>%
    tidyr::spread(key = .data$type, value = .data$data) %>%
    dplyr::filter(!stringr::str_detect(.data$eqn, filter_these)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      log_k = stringr::str_remove_all(.data$log_k, "\t|log_k") %>% stringr::str_trim(side = "both"),
      name = ifelse(stringr::str_detect(.data$name, "="), "", .data$name)
    ) %>%
    dplyr::arrange(.data$name, stringr::str_remove_all(.data$eqn, "[0-9]"))

  if(kable_format) {
    table %>%
      dplyr::mutate(
        eqn = stringr::str_replace_all(.data$eqn, "(\\+\\d)", "^\\1^") %>% # superscript charges
          stringr::str_replace_all("(\\-\\d)", "^\\1^") %>% # superscript charges
          stringr::str_replace_all("(\\w)(\\+)", "\\1^\\2^") %>% # superscript charges
          stringr::str_replace_all("(\\w)(\\-)", "\\1^\\2^") %>% # superscript charges
          stringr::str_replace_all("(\\))(\\d)", "\\1~\\2~") %>% # subscripts in formulas
          stringr::str_replace_all("(\\w)(\\d)", "\\1~\\2~"), # subscripts in formulas
        name = stringr::str_replace_all(.data$name, "(\\))(\\d)", "\\1~\\2~") %>%
          stringr::str_replace_all("([:alpha:])(\\d)", "\\1~\\2~") %>%
          stringr::str_replace("CuMetal", "Cu metal")
      )
  } else table
}
