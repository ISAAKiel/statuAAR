# Returns the measures for the formula abbreviation

Retreaves the bone measures needed out of the list with formula names
and bone measures for each formula.

## Usage

``` r
getFormulaMeasures(shortnames)
```

## Arguments

- shortnames:

  A vector of short names.

## Value

Vector with unique bone measure of the formula.

## Author

Christoph Rinne <crinne@ufg.uni-kiel.de>

## Examples

``` r
# Get the bone measures needed for the functions.
getFormulaMeasures(c('bb65', 'tg01'))
#> [1] "Hum2"  "Hum1"  "Rad1b" "Fem1"  "Tib1b" "Fib1"  "Uln1"  "Rad1" 
```
