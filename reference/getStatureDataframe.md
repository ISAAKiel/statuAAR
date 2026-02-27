# Returns the list of stature estimates from getStature() as data.frame.

Returns the data frames of the GetStature list with an additional column
for the formula used.

## Usage

``` r
getStatureDataframe(StatureList)
```

## Arguments

- StatureList, :

  the return of getStature().

## Value

A data.frame with eight columns:

- **formula**: short name of the formula, e.g. bb65,

- **ind**: individual identifyer (rownames),

- **sex**: as provided for calculation: m, f, indet.

- **stature**: estimated on the provided sex and bone measures,

- **bone measure name(s)**: bones used for calculation,

- **female stature**: columns with alternative stature for three sex
  classes,

- **male stature**,

- **indet. stature** and

- **n_measures**: number of bone measures included: e.g. 2 Fem2 (left,
  right) + 1 Tib1

## Author

Christoph Rinne <crinne@ufg.uni-kiel.de>

## Examples

``` r
# Get the bone measures needed for the functions.
getStatureDataframe(getStature(c('bb65', 'tg01'), dl.trotter.gleser))
#> Error in match.names(clabs, names(xi)): names do not match previous names
```
