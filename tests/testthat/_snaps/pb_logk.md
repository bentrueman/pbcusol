# pb_logk() generates the expected output.

    Code
      pb_logk()
    Output
      # A tibble: 23 x 3
         name  eqn                              log_k
         <chr> <chr>                            <dbl>
       1 ""    Pb+2 + CO3-2 + H+ = PbHCO3+      12.6 
       2 ""    Pb+2 + 2CO3-2 = Pb(CO3)2-2       10.3 
       3 ""    Pb+2 + CO3-2 = PbCO3              7.1 
       4 ""    Pb+2 + 2Cl- = PbCl2               1.8 
       5 ""    Pb+2 + Cl- = PbCl+                1.59
       6 ""    Pb+2 + 3Cl- = PbCl3-              1.71
       7 ""    Pb+2 + 4Cl- = PbCl4-2             1.43
       8 ""    Pb+2 + 2H2O = Pb(OH)2 + 2H+     -16.9 
       9 ""    3Pb+2 + 4H2O = Pb3(OH)4+2 + 4H+ -23.9 
      10 ""    4Pb+2 + 4H2O = Pb4(OH)4+4 + 4H+ -20.9 
      # ... with 13 more rows

---

    Code
      pb_logk(kable_format = TRUE)
    Output
      # A tibble: 23 x 3
         name  eqn                                          log_k
         <chr> <chr>                                        <dbl>
       1 ""    Pb^+2^ + CO~3~^-2^ + H^+^ = PbHCO~3~^+^      12.6 
       2 ""    Pb^+2^ + 2CO~3~^-2^ = Pb(CO~3~)~2~^-2^       10.3 
       3 ""    Pb^+2^ + CO~3~^-2^ = PbCO~3~                  7.1 
       4 ""    Pb^+2^ + 2Cl^-^ = PbCl~2~                     1.8 
       5 ""    Pb^+2^ + Cl^-^ = PbCl^+^                      1.59
       6 ""    Pb^+2^ + 3Cl^-^ = PbCl~3~^-^                  1.71
       7 ""    Pb^+2^ + 4Cl^-^ = PbCl~4~^-2^                 1.43
       8 ""    Pb^+2^ + 2H~2~O = Pb(OH)~2~ + 2H^+^         -16.9 
       9 ""    3Pb^+2^ + 4H~2~O = Pb~3~(OH)~4~^+2^ + 4H^+^ -23.9 
      10 ""    4Pb^+2^ + 4H~2~O = Pb~4~(OH)~4~^+4^ + 4H^+^ -20.9 
      # ... with 13 more rows

