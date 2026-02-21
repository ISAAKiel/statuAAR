# Returns a data.frame containing formula informations.

Returns a data.frame with three columns:

- short: short name for the formula, e.g. bb65, tg01.

- long: long name of the function name, e. g. breitinger_bach:1965,
  trotter_gleser_1952_an.

- measures: vector of measure names (Martin 1928), e.g. Hum1, Fem1,
  Tib1b

## Usage

``` r
getFormulaDataframe()
```

## Value

Data.frame with columns.

## Author

Christoph Rinne <crinne@ufg.uni-kiel.de>

## Examples

``` r
# Get the bone measures needed for the functions.
getFormulaDataframe()
#>    short                      long
#> 1   bb65      breitinger_bach_1965
#> 2   by89           byers_etal_1989
#> 3   fe90       feldesman_etal_1990
#> 4   ff96 formicola_franceschi_1996
#> 5   mn09    maijanen_niskanen_2009
#> 6   ol78         olivier_etal_1978
#> 7   pe99              pearson_1899
#> 8   ra08          raxter_etal_2008
#> 9   sj90              sjovold_1990
#> 10  te50              telkkae_1950
#> 11  tg01    trotter_gleser_1952_an
#> 12  tg02    trotter_gleser_1952_aw
#> 13  ve09     vercellotti_etal_2009
#> 14  r12a          ruff_etal_2012_a
#> 15  r12s          ruff_etal_2012_s
#> 16  r12n          ruff_etal_2012_n
#>                                                                               measures
#> 1                                                       Hum2, Hum1, Rad1b, Fem1, Tib1b
#> 2                                                                                 Mt11
#> 3                                                                                 Fem1
#> 4                                                    Fem2+Tib1, Fem1, Tib1, Hum1, Rad1
#> 5                       Fem2+Tib1, Fem1+Tib1, Fem2, Fem1, Tib1, Hum1, Rad1, Fib1, Uln1
#> 6                                                       Hum2, Hum1, Rad1b, Fem1, Tib1b
#> 7                                                       Hum1, Rad1, Fem1, Tib1b, Tib1a
#> 8  Fem1, Fem1+Tib1a, Fem2, Fem2+Tib1a, Fem2+Tib1b, Hum1, Hum1+Rad1, Rad1, Tib1a, Tib1b
#> 9                               Hum1, Rad1, Rad1b, Uln1, Fem1, Fem2, Tib1, Tib1b, Fib1
#> 10                                                  Hum1, Rad2, Uln2, Fem1, Tib1, Fib1
#> 11                                                 Fem1, Tib1b, Fib1, Uln1, Rad1, Hum1
#> 12                                                 Fem1, Tib1b, Fib1, Uln1, Rad1, Hum1
#> 13                                  Fem2+Tib1, Fem2, Fem1, Tib1, Hum1+Rad1, Hum1, Rad1
#> 14                                                                    Fem1, Hum1, Rad1
#> 15                                                                     Fem1+Tib1, Tib1
#> 16                                                                     Fem1+Tib1, Tib1
```
