test_that("change_logk() makes expected modification to a database", {
  logk_new <- 23.999
  modified <- change_logk(db = phreeqc::minteq.v4.dat, eqn = "Pb+2 + Cl- = PbCl+", logk = logk_new)[1820:1821]
  logk_recovered <- stringr::str_extract(modified[2], "\\d+\\.?\\d+$")
  expect_equal(logk_new, as.numeric(logk_recovered))
})

test_that("change_logk() replaces log Ks in default database with those in phreeqc::minteq.v4.dat", {
  rewrite_hydcerussite <- function(x) {
    stringr::str_replace(
      x,
      modify_eqn("Pb3(OH)2(CO3)2 + 2H+ = 3Pb+2 + 2H2O + 2CO3-2"),
      "Pb(OH)2:2PbCO3 + 2H+ = 3Pb+2 + 2CO3-2 + 2H2O"
    )
  }
  # set up args to change_logk():
  # (these are the log Ks from minteq.v4 that will replace those in the default database)
  db_revision <- pb_logk(db = phreeqc::minteq.v4.dat)
  db_revision$eqn <- rewrite_hydcerussite(db_revision$eqn)
  remove_these <- c( # not present in minteq.v4.dat
    "6Pb+2 + 8H2O = Pb6(OH)8+4 + 8H+",
    "Pb+2 + PO4-3 + H+ = PbHPO4",
    "Pb+2 + PO4-3 + 2H+ = PbH2PO4+"
  )
  target <- pb_logk(db = phreeqc::minteq.v4.dat)
  target$eqn <- rewrite_hydcerussite(target$eqn)
  modified <- pb_logk(db = with(db_revision, change_logk(eqn = eqn, logk = log_k)))
  modified <- modified[!modified$eqn %in% remove_these, ]
  expect_equal(target[order(target$log_k), -1], modified[order(modified$log_k), -1])
})
