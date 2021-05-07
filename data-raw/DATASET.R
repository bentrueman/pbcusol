## code to prepare `DATASET` dataset goes here

library("tidyverse")

write(pbcusol:::leadsol, "data-raw/leadsol.txt")
write(pbcusol:::cu2sol, "data-raw/cu2sol.txt")

leadsol <- pbcusol:::leadsol
cu2sol <- pbcusol:::cu2sol

usethis::use_data(leadsol, cu2sol, internal = TRUE, overwrite = TRUE)

