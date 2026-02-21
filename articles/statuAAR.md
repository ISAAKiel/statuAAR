# Introduction to statuAAR

## Get started

Get a df of the available formula and measures.

``` r
statuAAR::getFormulaDataframe()
```

    ##    short                      long
    ## 1   bb65      breitinger_bach_1965
    ## 2   by89           byers_etal_1989
    ## 3   fe90       feldesman_etal_1990
    ## 4   ff96 formicola_franceschi_1996
    ## 5   mn09    maijanen_niskanen_2009
    ## 6   ol78         olivier_etal_1978
    ## 7   pe99              pearson_1899
    ## 8   ra08          raxter_etal_2008
    ## 9   sj90              sjovold_1990
    ## 10  te50              telkkae_1950
    ## 11  tg01    trotter_gleser_1952_an
    ## 12  tg02    trotter_gleser_1952_aw
    ## 13  ve09     vercellotti_etal_2009
    ## 14  r12a          ruff_etal_2012_a
    ## 15  r12s          ruff_etal_2012_s
    ## 16  r12n          ruff_etal_2012_n
    ##                                                                               measures
    ## 1                                                       Hum2, Hum1, Rad1b, Fem1, Tib1b
    ## 2                                                                                 Mt11
    ## 3                                                                                 Fem1
    ## 4                                                    Fem2+Tib1, Fem1, Tib1, Hum1, Rad1
    ## 5                       Fem2+Tib1, Fem1+Tib1, Fem2, Fem1, Tib1, Hum1, Rad1, Fib1, Uln1
    ## 6                                                       Hum2, Hum1, Rad1b, Fem1, Tib1b
    ## 7                                                       Hum1, Rad1, Fem1, Tib1b, Tib1a
    ## 8  Fem1, Fem1+Tib1a, Fem2, Fem2+Tib1a, Fem2+Tib1b, Hum1, Hum1+Rad1, Rad1, Tib1a, Tib1b
    ## 9                               Hum1, Rad1, Rad1b, Uln1, Fem1, Fem2, Tib1, Tib1b, Fib1
    ## 10                                                  Hum1, Rad2, Uln2, Fem1, Tib1, Fib1
    ## 11                                                 Fem1, Tib1b, Fib1, Uln1, Rad1, Hum1
    ## 12                                                 Fem1, Tib1b, Fib1, Uln1, Rad1, Hum1
    ## 13                                  Fem2+Tib1, Fem2, Fem1, Tib1, Hum1+Rad1, Hum1, Rad1
    ## 14                                                                    Fem1, Hum1, Rad1
    ## 15                                                                     Fem1+Tib1, Tib1
    ## 16                                                                     Fem1+Tib1, Tib1

Check Breitinger & Bach 1965 documentation.

``` r
?statuAAR::breitinger_bach_1965
```

Load some data, e.g. Bach1965, into a data.frame.

``` r
Bach1965 <- statuAAR::Bach1965
head(Bach1965)
```

    ##   sex Hum1 Hum2 Rad1b stature Fem1 Tib1b
    ## 1   f  220  215   146    1450  292   281
    ## 2   f  224  219   151    1460  299   287
    ## 3   f  229  224   156    1470  307   293
    ## 4   f  233  229   162    1480  315   298
    ## 5   f  239  234   167    1490  322   301
    ## 6   f  243  238   172    1500  330   310

Prepare tabled data into a long list (statuaar_data_table).

``` r
dl.bach1965 <- statuAAR::prep.statuaar.data(Bach1965, d.form = "wide",
                   measures.names = "short", sex = "sex", stats = FALSE)
```

    ## Warning in statuAAR::prep.statuaar.data(Bach1965, d.form = "wide",
    ## measures.names = "short", : No individual identifier provided, each record
    ## (row) will be counted as one individual.

Check the measurements using general statistics.

``` r
statuAAR::measures.statistics(dl.bach1965)
```

    ## # A tibble: 5 × 8
    ##   measure     n  MinM Quart1 MedianM MeanM Quart3  MaxM
    ##   <chr>   <dbl> <dbl>  <dbl>   <dbl> <dbl>  <dbl> <dbl>
    ## 1 Fem1       64   292    382     436   434    490   557
    ## 2 Hum1       64   220    278     311   309    343   387
    ## 3 Hum2       64   215    272     304   303    337   379
    ## 4 Rad1b      64   146    204     238   235    270   302
    ## 5 Tib1b      64   281    328     371   371    414   455

Get the stature estimations of the provided measurements. The function
returns a list with a data frame for each formula.

``` r
head(statuAAR::getStature(c('bb65'), dl.bach1965)$bb65)
```

    ##    sex stature                     bone female male indet n_measures
    ## 32   m    1540 Hum2, Rad1b, Fem1, Tib1b   1531 1540  1536          4
    ## 33   m    1550 Hum2, Rad1b, Fem1, Tib1b   1538 1550  1544          4
    ## 34   m    1560 Hum2, Rad1b, Fem1, Tib1b   1546 1560  1553          4
    ## 35   m    1570 Hum2, Rad1b, Fem1, Tib1b   1554 1570  1562          4
    ## 36   m    1580 Hum2, Rad1b, Fem1, Tib1b   1561 1580  1571          4
    ## 37   m    1590 Hum2, Rad1b, Fem1, Tib1b   1570 1590  1580          4
