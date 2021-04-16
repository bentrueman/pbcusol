
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pbcusol

<!-- badges: start -->

<!-- badges: end -->

`pbcusol` predicts equilibrium lead and copper solubility using the US
EPA databases LEADSOL (Schock et al. 1996) and CU2SOL (Schock et al.,
1995), along with those available as a part of PHREEQC (Charlton and
Parkhurst, 2011; Parkhurst and Appelo, 2013). `pbcusol` uses
`tidyphreeqc` (Dunnington, 2019)—a convenient interface for PHREEQC in
R—for most solubility computations.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("bentrueman/pbcusol")
```

## Example

For this example, you will need the `tidyverse` family of packages,
along with `plyr::round_any()`, and `viridis::scale_fill_viridis()`.

``` r
library("pbcusol")
library("tidyverse")
library("viridis")
```

Use `cu_sol()` to calculate the equlibrium solubility of multiple copper
phases that occur in drinking water systems, over a wide range of pH
values and dissolved inorganic carbon concentrations. (N.B., evaluate on
a smaller grid to speed this up\!)

``` r
dic_increment_cu <- 1.5
solutions_cu <- list("Cu(OH)2", "Tenorite", "Malachite") %>%
  set_names() %>%
  map_dfr(
    ~cu_sol(
      ph = seq(6, 11, by = .025),
      dic = seq(1, 150, by = dic_increment_cu),
      phase = .x
    ),
    .id = "phase"
  )
```

Plot the data (n.b., plots shown here will differ slightly from those
the following code generates). Lytle et al. (2018) have noted that
copper solubility predictions are not reliable in the presence of
orthophosphate, and they propose an empirical model as an alternative.
It is implemented in `cu_sol()` via the argument `empirical = TRUE`.

``` r
solutions_cu %>% 
  mutate(
    log10_cu_ppb = log10(cu_ppb),
    dic_ppm = plyr::round_any(dic_ppm, dic_increment_cu)
  ) %>% 
  ggplot(aes(x = dic_ppm, y = pH)) + 
  facet_wrap(vars(phase)) + 
  geom_raster(aes(fill = log10_cu_ppb)) +
  geom_contour(aes(z = log10_cu_ppb), col = "white") +
  viridis::scale_fill_viridis(option = "magma")
```

<img src="man/figures/README-cu-plot-1.png" width="100%" /> Use
`pb_sol()` to make the same predictions for lead. The helper function
`calculate_dic()` may be useful for converting alkalinity to dissolved
inorganic carbon.

``` r
dic_increment_pb <- .8
solutions_pb <- list("Cerussite", "Hydcerussite", "Hxypyromorphite") %>%
  set_names() %>%
  map_dfr(
    ~ pb_sol(
      ph = seq(6, 11, by = .025),
      dic = seq(1, 80, by = dic_increment_pb),
      phosphate = .16,
      phase = .x
    ),
    .id = "phase"
  )
```

Plot the data:

``` r
solutions_pb %>% 
  mutate(
    log10_pb_ppb = log10(pb_ppb),
    dic_ppm = plyr::round_any(dic_ppm, dic_increment_pb)
  ) %>% 
  ggplot(aes(x = dic_ppm, y = pH)) + 
  facet_wrap(vars(phase)) + 
  geom_raster(aes(fill = log10_pb_ppb)) +
  geom_contour(aes(z = log10_pb_ppb), col = "white") +
  viridis::scale_fill_viridis(option = "magma")
```

<img src="man/figures/README-pb-plot-1.png" width="100%" />

Combining the information in the previous two plots, we can generate
prediction surfaces for equilibrium lead and copper solubility that are
quite close to those presented in literature, specifically Figure 7 in
Schock et al. (1995) and Figure 4-18 in Schock et al. (1996).

<img src="man/figures/README-combined-1.png" width="100%" /> Finally,
use `pb_logk()` and `cu_logk()` to generate tables of the relevant
reactions and constants in the LEADSOL and CU2SOL databases. N.B.,
`pbcusol` uses modified versions of `phreeqc::minteq.dat` that include
the data from LEADSOL and CU2SOL. `phreeqc::minteq.dat` includes two
reactions describing complexation of copper and chloride that are not
included in Schock et al. (1995).

# References

Charlton, S.R., and D. L. Parkhurst, 2011, Modules based on the
geochemical model PHREEQC for use in scripting and programming
languages: Computers & Geosciences, v. 37, p. 1653-1663.

Dunnington, D. 2019. tidyphreeqc: Tidy Geochemical Modeling Using
PHREEQC. <https://github.com/paleolimbot/tidyphreeqc>.

Lytle, D. A., M. R. Schock, J. Leo, and B. Barnes. 2018. A model for
estimating the impact of orthophosphate on copper in water. Journal
AWWA. 110: E1-E15.
<https://doi-org.ezproxy.library.dal.ca/10.1002/awwa.1109>

Parkhurst, D. L., and C. A. J. Appelo. 2013. Description of input and
examples for PHREEQC version 3–A computer program for speciation, batch-
reaction, one-dimensional transport, and inverse geochemical
calculations: U.S. Geological Survey Techniques and Methods, book 6,
chap. A43, 497 p. <http://pubs.usgs.gov/tm/06/a43>.

Schock, M. R., D. A Lytle, and J. A. Clement. 1995. “Effect of pH, DIC,
orthophosphate and sulfate on drinking water cuprosolvency.” National
Risk Management Research Lab., Cincinnati, OH (United States).

Schock, M. R., I. Wagner, and R. J. Oliphant. 1996. “Corrosion and
solubility of lead in drinking water.” In Internal corrosion of water
distribution systems, 2nd ed., 131–230. Denver, CO: American Water Works
Association Research Foundation.
