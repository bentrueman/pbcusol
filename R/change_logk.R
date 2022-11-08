
#' Change log K values in a database
#'
#' @param db The PHREEQC database to modify.
#' @param eqn A character vector of equations to modify.
#' @param logk A character vector containing the replacement log K values, corresponding element-wise to `eqn`.
#'
#' @return A modified PHREEQC database.
#' @importFrom dplyr %>%
#' @importFrom stringr str_replace_all str_detect str_replace
#' @export
#'
#' @examples
#' phreeqc::minteq.v4.dat[1820:1821]
#' change_logk(db = phreeqc::minteq.v4.dat, eqn = "Pb+2 + Cl- = PbCl+", logk = 1.59)[1820:1821]
change_logk <- function(db = pbcusol:::pbcu2sol, eqn, logk) {
  validate_db(db)
  stopifnot("arguments 'eqn' and 'logk' must be equal in length" = length(eqn) == length(logk))
  eqn <- modify_eqn(eqn)
  for(i in seq_len(length(eqn))) {
    # rowids of log_ks to replace
    log_k_index <- which(str_detect(db, eqn[i])) + 1
    # extract log_ks to replace:
    log_k_old <- db[log_k_index]
    # modify them:
    log_k_new <- str_replace(log_k_old, "[^\\s]+$", as.character(logk[i]))
    # replace old with new
    db[log_k_index] <- log_k_new
  }

  db

}

modify_eqn <- function(x) {
  x %>%
    str_replace_all("\\+", "\\\\+") %>%
    str_replace_all("\\(", "\\\\(") %>%
    str_replace_all("\\)", "\\\\)")
}

validate_db <- function(db) {
  eqn_index <- which(str_detect(db, "=") & !str_detect(db, "^#"))
  # extract lines after equations:
  after_eqn <- db[eqn_index + 1]
  logk_after_eqn <- str_detect(after_eqn, "log_k") | str_detect(after_eqn, "^#")
  stopifnot("Throughout the database, log Ks must appear on the lines following each equation." = mean(logk_after_eqn) == 1)
}
