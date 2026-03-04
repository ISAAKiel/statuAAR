# Calculate stature estimation according to: Byers et al 1989.

Stature estimation (mm) based on a regression calculation of one
Metatarsal 1 only separated by sex (Byers et al 1989). Bone measures
used: Os metatarsale I, apex of the capitulum to the midpoint of the
articular surface of the base parallel to the longituninal axis of the
bone.

If bone measures for left and right are provided the mean value will be
used, but for statistic information 2 bones will be counted
(n_measures). For archaeological finds, group assignment (Euro-American
or Afro-American) is not possible. In addition, the gender-specific
estimates for MtI1 offer the best accuracy. For this reason, only the
following formulas are used:

- Combined data (indet): stature = 634 + 16.8 (Metl),

- All males: stature = 815 + 14.3 (Metl),

- All females: stature = 783 + 13.9 (Metl).

Returns a data.frame with:

- ind: individual identifyer (rownames),

- sex: as provided for calculation: m, f, indet.

- stature: estimated on the provided sex and bone measures,

- bone (measure(s)): bones used for calculation,

- if_female (stature): columns with alternative stature for three sex
  classes,

- if_male (stature),

- if_indet. (stature) and

- n_measures: number of bone measures included: e.g. 2 MtI1 (left,
  right)

## Usage

``` r
byers_etal_1989(df)
```

## Arguments

- df:

  data.frame of type statuaar_data_table, containing informations on
  individual, bone and measurement.

## Value

data.frame with calculated stature and related information per
individual.

## References

Byers S, Akoshima K, Curran B (1989). “Determination of adult stature
from metatarsal length.” *American Journal of Physical Anthropology*,
**79**(3), 275–279. ISSN 1096-8644,
[doi:10.1002/ajpa.1330790303](https://doi.org/10.1002/ajpa.1330790303) .

## Author

Christoph Rinne <crinne@ufg.uni-kiel.de>

## Examples

``` r
# Read example dataset into a data frame
x <- as.data.frame(list(sex = c("f", "m", "i"), MtI1 = c(49, 50, 51)))

# Prepare tabled data into a long list (statuaar_data_table)
dl.by89 <- statuAAR::prep.statuaar.data(x, d.form = "wide",
                       measures.names = "short", sex = "sex", stats = FALSE)
#> Warning: No individual identifier provided, each record (row) will be counted as one individual.

# Calculate stature estimation using a given formula.
by89.estimates <- statuAAR::getStature(c("by89"), dl.by89)

# Extract the corresponding data frame from the returned list object.
by89.estimates[["by89"]]
#>     sex stature bone if_female if_male if_indet n_measures
#> 2     m    1530 MtI1      1478    1530     1474          1
#> 1     f    1464 MtI1      1464    1516     1457          1
#> 3 indet    1491 MtI1      1492    1544     1491          1
```
