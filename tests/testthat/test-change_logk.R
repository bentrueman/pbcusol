test_that("change_logk() makes expected modification to a database", {
  logk_new <- 23.999
  modified <- change_logk(db = phreeqc::minteq.v4.dat, eqn = "Pb+2 + Cl- = PbCl+", logk = logk_new)
  these_rows <- which(stringr::str_detect(modified, "Pb\\+2 \\+ Cl- = PbCl\\+"))
  these_rows <- c(these_rows, these_rows + 1)
  modified <- modified[these_rows]
  logk_recovered <- stringr::str_extract(modified[2], "\\d+\\.?\\d+$")
  expect_equal(logk_new, as.numeric(logk_recovered))
})

test_that("change_logk() replaces log Ks in default database with those in phreeqc::minteq.v4.dat", {
  rewrite_hydcerussite <- function(x) {
    stringr::str_replace(
      x,
      modify_eqn("Pb3(OH)2(CO3)2 + 2 H+ = 3 Pb+2 + 2 H2O + 2 CO3-2"),
      "Pb(OH)2:2PbCO3 + 2 H+ = 3 Pb+2 + 2 CO3-2 + 2 H2O"
    )
  }
  # set up args to change_logk():
  # (these are the log Ks from minteq.v4 that will replace those in the default database)
  db_revision <- pb_logk(db = phreeqc::minteq.v4.dat)
  db_revision$eqn <- rewrite_hydcerussite(db_revision$eqn)
  # remove spaces between coefficients and elements:
  db_revision$eqn <- stringr::str_remove_all(db_revision$eqn, "(?<=\\d) (?=[A-Z])")
  remove_these <- c( # not present in minteq.v4.dat
    "6Pb+2 + 8H2O = Pb6(OH)8+4 + 8H+",
    "Pb+2 + PO4-3 + H+ = PbHPO4",
    "Pb+2 + PO4-3 + 2H+ = PbH2PO4+"
  )
  target <- pb_logk(db = phreeqc::minteq.v4.dat)
  target$eqn <- rewrite_hydcerussite(target$eqn)
  # remove spaces between coefficients and elements:
  target$eqn <- stringr::str_remove_all(target$eqn, "(?<=\\d) (?=[A-Z])")
  modified <- pb_logk(db = with(db_revision, change_logk(eqn = eqn, logk = log_k)))
  modified <- modified[!modified$eqn %in% remove_these, ]
  expect_equal(target[order(target$log_k), -1], modified[order(modified$log_k), -1])
})

test_that("change_logk() informs when equation not found in database", {
  expect_error(
    change_logk(eqn = c("not an equation", "also not an equation"), logk = 1:2),
    regexp = "not found in database"
  )
})
