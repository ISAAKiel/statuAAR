# Calculate stature estimation according to: Trotter & Gleser 'American Negro'.

Stature estimation (mm) based on the hierarchy of different regression
calculations, separated by sex (Trotter & Gleser 1952, 1977) series
'American Negro'. Bone measures used: Fem1, Tib1b, Fib1, Uln1, Rad1,
Hum1 If bone measures for left and right are provided the mean value
will be used, but for statistic information 2 bones will be counted
(n_measures). If sex is indet. the mean of male and female stature
estimation is given. The calculation is based on Trotter & Gleser (1952)
with corrections from Trotter & Gleser (1977). The regression formula
are hierarchical from combinations of different bone measures to single
bone measures. Only the first applicable measure will be used.

Returns a data.frame with:

- ind: individual identifyer (rownames),

- sex: as provided for calculation: m, f, indet.

- stature: estimated on the provided sex and bone measures,

- bone (measure(s)): bones used for calculation,

- female (stature): columns with alternative stature for three sex
  classes,

- male (stature),

- indet. (stature) and

- n_measures: number of bone measures included: e.g. 2 Fem2 (left,
  right) + 1 Tib1

## Usage

``` r
trotter_gleser_1952_an(df)
```

## Arguments

- df:

  data.frame of type statuaar_data_table, containing informations on
  individual, bone and measurement.

## Value

data.frame with calculated stature and related information per
individual.

## References

Trotter M, Gleser GC (1952). “Estimation of stature from long bones of
American Whites and Negroes.” *American Journal of Physical
Anthropology*, **10**(4), 463–514. ISSN 1096-8644,
[doi:10.1002/ajpa.1330100407](https://doi.org/10.1002/ajpa.1330100407) .

Trotter M, Gleser GC (1977). “Corrigenda to “estimation of stature from
long limb bones of American Whites and Negroes,” American Journal
Physical Anthropology (1952).” *American Journal of Physical
Anthropology*, **47**(2), 355–356. ISSN 1096-8644,
[doi:10.1002/ajpa.1330470216](https://doi.org/10.1002/ajpa.1330470216) .

## Author

Hendrik Raese <h.raese@ufg.uni-kiel.de>

Christoph Rinne <crinne@ufg.uni-kiel.de>

## Examples

``` r
# Read example dataset into a data frame
```
