# Calculate stature estimation based on bone measures according to: Formicola & Franceschi 1996.

Stature estimation (mm) based on a hierarchical order of regression
calculations, separated by sex and based on: Fem2+Tib1, Fem1, Tib1,
Hum1, Rad1. If bone measures for left and right are provided the mean
value will be used, but for statistic information 2 bones will be
counted (n_measures). If sex is indet. the mean of male and female
stature estimation is given. Formicola and Franceschi (1996) do not
provide information on hierarchie or mean calculation of the regression
formula, but table 3 gives standard error (S.E.) and correlation from
which a hierarchical order can be derived. The order of bone measures
used according to achieved correlation differs between sex and will be
respected, allthough differences are small. Due to this, the order used
differs slightly from Siegmund (2010, 116 (12.8)).

Bone measures used: Fem2+Tib1, Tib1, Fem1, Hum1, Rad1 (or Rad1a)

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
  right), 1 Tib1

## Usage

``` r
formicola_franceschi_1996(df)
```

## Arguments

- df:

  data.frame of type statuaar_data_table, containing informations on
  individual, bone and measurement.

## Value

data.frame with calculated stature and related information per
individual.

## References

Formicola V, Franceschi M (1996). “Regression equations for estimating
stature from long bones of early Holocene European samples.” *American
Journal of Physical Anthropology*, **100**(1), 83–88. ISSN 1096-8644,
[doi:10.1002/(SICI)1096-8644(199605)100:1\\3C83::AID-AJPA8\\3E3.0.CO;2-E](https://doi.org/10.1002/%28SICI%291096-8644%28199605%29100%3A1%5C%253C83%3A%3AAID-AJPA8%5C%253E3.0.CO%3B2-E)
.

Formicola V (1993). “Stature reconstruction from long bones in ancient
population samples: An approach to the problem of its reliability.”
*American Journal of Physical Anthropology*, **90**(3), 351–358. ISSN
1096-8644,
[doi:10.1002/ajpa.1330900309](https://doi.org/10.1002/ajpa.1330900309) .

Siegmund F (2010). *Die Körpergröße der Menschen in der Ur- und
Frühgeschichte Mitteleuropas und ein Vergleich ihrer anthropologischen
Schätzmethoden*. Books on Demand, Norderstedt. ISBN 978-3-8391-5314-7,
tex.ids: siegmundKorpergrosseMenschenUr2010a.

## Author

Christoph Rinne <crinne@ufg.uni-kiel.de>

## Examples

``` r
# Read example dataset into a data frame
```
