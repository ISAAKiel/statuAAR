# Returns the long names of the formula abbreviation

Retreaves the long names of the formula abbreviations out of the list
with names and bone measures for each formula.

## Usage

``` r
getFormulaNames(shortnames)
```

## Arguments

- shortnames:

  A vector of short names.

## Value

Vector with long names of the formula.

## Author

Christoph Rinne <crinne@ufg.uni-kiel.de>

## Examples

``` r
# Get the long names of the function abbreviations
getFormulaNames(c('bb65', 'tg01'))
#> [1] "breitinger_bach_1965"   "trotter_gleser_1952_an"
```
