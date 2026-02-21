# Calculate stature estimation according to: Vercellotti et al. 2009

Stature estimation (mm) based on the hierarchy of different regression
calculations, separated by sex (Vercellotti et al. 2009). Bone measures
used: Fem2+Tib1, Fem2, Fem1, Tib1, Hum1+Rad1, Hum1, Rad1

If bone measures for left and right are provided the mean value will be
used, but for statistic information 2 bones will be counted
(n_measures). Vercellotti et al. propose regression calculations for the
combination of male and female individuals. These regressions will be
used in case of undetermined sex (indet.).

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
vercellotti_etal_2009(df)
```

## Arguments

- df:

  data.frame of type statuaar_data_table, containing informations on
  individual, bone and measurement.

## Value

data.frame with calculated stature and related information per
individual.

## References

Vercellotti G, Agnew AM, Justus HM, Sciulli PW (2009). “Stature
estimation in an early medieval (XI-XII c.) Polish population: Testing
the accuracy of regression equations in a bioarcheological sample.”
*American Journal of Physical Anthropology*, **140**(1), 135–142. ISSN
1096-8644, [doi:10.1002/ajpa.21055](https://doi.org/10.1002/ajpa.21055)
.

## Author

Anna Loy <aloy@roots.uni-kiel.de>

Nils Müller-Scheeßel <nils.mueller-scheessel@ufg.uni-kiel.de>

Hendrik Raese <h.raese@ufg.uni-kiel.de>

Christoph Rinne <crinne@ufg.uni-kiel.de>

## Examples

``` r
# Read example dataset into a data frame
```
