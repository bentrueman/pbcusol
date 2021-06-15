## code to prepare `pbcu2sol` dataset goes here

library("tidyverse")

pbcu2sol <- read_lines("data-raw/pbcu2sol.txt")
jurgens <- read_lines("data-raw/jurgens_et_al.txt")

usethis::use_data(pbcu2sol, jurgens, internal = TRUE, overwrite = TRUE)

