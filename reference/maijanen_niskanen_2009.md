# Calculate stature estimation according to: Maijanen, Niskanen 2009

Stature estimation (mm) based on the hierarchy of different regression
calculations, separated by sex (Maijanen, Niskanen 2009). Bone measures
used: Fem2+Tib1, Fem1+Tib1, Fem2, Fem1, Tib1, Hum1, Rad1, Fib1, Uln1

If bone measures for left and right are provided the mean value will be
used, but for statistic information 2 bones will be counted
(n_measures). Maijanen & Niskanen propose to use first the combination
of femur and tibia, followed by calculations on the hierarchie of single
measures. They present regression calculations for the combination of
male and female individuals recomended in case of undetermined sex
(indet.).

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
maijanen_niskanen_2009(df)
```

## Arguments

- df:

  data.frame of type statuaar_data_table, containing informations on
  individual, bone and measurement.

## Value

data.frame with calculated stature and related information per
individual.

## References

Maijanen H, Niskanen M (2010). “New regression equations for stature
estimation for medieval Scandinavians.” *International Journal of
Osteoarchaeology*, **20**(4), 472–480. ISSN 1099-1212,
[doi:10.1002/oa.1071](https://doi.org/10.1002/oa.1071) .

## Author

Anna Loy <aloy@roots.uni-kiel.de>

Nils Müller-Scheeßel <nils.mueller-scheessel@ufg.uni-kiel.de>

Hendrik Raese <h.raese@ufg.uni-kiel.de>

Christoph Rinne <crinne@ufg.uni-kiel.de>

## Examples

``` r
# Read example dataset into a data frame
```
