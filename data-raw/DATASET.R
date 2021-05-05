## code to prepare `DATASET` dataset goes here

library("tidyverse")

write(pbcusol:::leadsol, "data-raw/leadsol.txt")
write(pbcusol:::cu2sol, "data-raw/cu2sol.txt")

# create lead2sol by adding variscite to leadsol from https://doi.org/10.1016/j.gca.2010.10.012

leadsol <- pbcusol:::leadsol
cu2sol <- pbcusol:::cu2sol

lead2sol <- read_lines("data-raw/lead2sol.txt")

usethis::use_data(leadsol, lead2sol, cu2sol, internal = TRUE, overwrite = TRUE)

