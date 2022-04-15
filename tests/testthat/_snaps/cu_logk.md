# pb_logk() generates the expected output.

    Code
      cu_logk()
    Output
      # A tibble: 53 x 3
         name  eqn                                      log_k 
         <chr> <chr>                                    <chr> 
       1 ""    Cu+2 + CO3-2 + H+ = CuHCO3+              12.13 
       2 ""    Cu+2 + CO3-2 + 2H2O = Cu(OH)2CO3-2 + 2H+ -13.14
       3 ""    Cu+2 + CO3-2 + H2O = Cu(OH)CO3- + H+     -4.25 
       4 ""    Cu+2 + 2CO3-2 = Cu(CO3)2-2               10.60 
       5 ""    Cu+2 + CO3-2 = CuCO3                     6.82  
       6 ""    Cu+ + Cl- = CuCl                         2.7   
       7 ""    Cu+2 + 2Cl- = CuCl2                      -0.12 
       8 ""    Cu+2 + Cl- = CuCl+                       0.40  
       9 ""    2Cu+ + 4Cl- = Cu2Cl4-2                   10.32 
      10 ""    Cu+ + 2Cl- = CuCl2-                      5.48  
      # ... with 43 more rows

---

    Code
      cu_logk(kable_format = TRUE)
    Output
      # A tibble: 53 x 3
         name  eqn                                                      log_k 
         <chr> <chr>                                                    <chr> 
       1 ""    Cu^+2^ + CO~3~^-2^ + H^+^ = CuHCO~3~^+^                  12.13 
       2 ""    Cu^+2^ + CO~3~^-2^ + 2H~2~O = Cu(OH)~2~CO~3~^-2^ + 2H^+^ -13.14
       3 ""    Cu^+2^ + CO~3~^-2^ + H~2~O = Cu(OH)CO~3~^-^ + H^+^       -4.25 
       4 ""    Cu^+2^ + 2CO~3~^-2^ = Cu(CO~3~)~2~^-2^                   10.60 
       5 ""    Cu^+2^ + CO~3~^-2^ = CuCO~3~                             6.82  
       6 ""    Cu^+^ + Cl^-^ = CuCl                                     2.7   
       7 ""    Cu^+2^ + 2Cl^-^ = CuCl~2~                                -0.12 
       8 ""    Cu^+2^ + Cl^-^ = CuCl^+^                                 0.40  
       9 ""    2Cu^+^ + 4Cl^-^ = Cu~2~Cl~4~^-2^                         10.32 
      10 ""    Cu^+^ + 2Cl^-^ = CuCl~2~^-^                              5.48  
      # ... with 43 more rows

