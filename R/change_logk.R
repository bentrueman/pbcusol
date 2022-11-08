
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
  eqn_rx <- modify_eqn(eqn)
  eqn_len <- length(eqn_rx)
  index <- seq_len(eqn_len)
  index_length <- vector(mode = "integer", length = eqn_len)
  for(i in index) {
    # rowids of log_ks to replace
    log_k_index <- which(str_detect(db, eqn_rx[i])) + 1
    # extract log_ks to replace:
    log_k_old <- db[log_k_index]
    # modify them:
    log_k_new <- str_replace(log_k_old, "[^\\s]+$", as.character(logk[i]))
    # replace old with new
    db[log_k_index] <- log_k_new

    index_length[i] <- length(log_k_index)
  }

  if (any(index_length == 0)) {
    stop(paste0(eqn[which(index_length == 0)], " not found in database."))
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
  stopifnot("Each log K must appear on the line following the equation at applies to." = mean(logk_after_eqn) == 1)
}
