## code to prepare `pbcu2sol` dataset goes here

library("tidyverse")

pbcu2sol <- read_lines("data-raw/pbcu2sol.txt")

usethis::use_data(pbcu2sol, internal = TRUE, overwrite = TRUE)

