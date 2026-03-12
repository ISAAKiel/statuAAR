# Calculate stature estimation based on bone measures according to: Formicola & Franceschi 1996.

Stature estimation (mm) based on a hierarchical order of regression
calculations, separated by sex and based on: Fem2 + Tib1, Fem1, Tib1,
Hum1, Rad1. If bone measures for left and right are provided the mean
value will be used, but for statistic information 2 bones will be
counted (n_measures). If sex is indet. the mean of male and female
stature estimation is given. Formicola and Franceschi (1996) do not
provide information on hierarchie or mean calculation of the regression
formula, but table 3 gives standard error (S.E.) and correlation from
which a hierarchical order can be derived. The order of bone
measurements is identical for men and women for the first three
formulas, although the respective SE differ significantly: Fem2 + Tib1,
Fem1, Tib1. For women, Hum1 and Rad1 follow with a significant
difference in SE, while for men, with a small SE difference, Rad1
follows first and then Hum1 (1996, Tab. 3). For individuals of
indeterminate sex, stature estimatation is only possible by disregarding
the hierarchical order or by taking the mean of two different bone
measurements. For this reason, the hierarchical order of formulas for
women is used for all individuals. In addition, the hierarchical order
is given for the least-square linear regression, allthough regressions
using the major axis of the correlation plane are applied (1996, 83, 86
Tab. 3). Due to this, the order used differs slightly from Siegmund
(2010, 116 (12.8)).

Bone measures used: Fem2 + Tib1 (both present), Tib1, Fem1, Hum1, Rad1
(or Rad1a). The addition of individual bone measurements is performed
during the calculation.

Returns a data.frame with:

- ind: individual identifyer (rownames),

- sex: as provided for calculation: m, f, indet.

- stature: estimated on the provided sex and bone measures,

- bone (measure(s)): bones used for calculation,

- if_female (stature): columns with alternative stature for three sex
  classes,

- if_male (stature),

- if_indet. (stature) and

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
x <- statuAAR::Bach1965

# Prepare tabled data into a long list (statuaar_data_table)
dl.bach1965 <- statuAAR::prep.statuaar.data(x, d.form = "wide",
                       measures.names = "short", sex = "sex", stats = FALSE)
#> Warning: No individual identifier provided, each record (row) will be counted as one individual.
#> Error in loadNamespace(x): there is no package called ‘tidyr’

# Calculate stature estimation using a given formula.
ff96.estimates <- statuAAR::getStature(c("ff96"), dl.bach1965)
#> Error: object 'dl.bach1965' not found

# Extract the corresponding data frame from the returned list object.
ff96.estimates[["ff96"]]
#> Error: object 'ff96.estimates' not found
```
