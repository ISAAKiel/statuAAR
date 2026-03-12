# Calculate stature estimation according to: Boldsen 1984

Stature estimation (mm) based on the mean of regression calculations,
separated by sex (Boldsen 1984). Bone measures used: Fem1, Tib1

If bone measures for left and right are provided the mean value will be
used, but for statistic information 2 bones will be counted
(n_measures). If sex is indet. the mean of male and female stature
estimation is given.

From a skeleton series of approximately 500 individuals from the
medieval burial ground of Lille Sct. Mikkelsgade in Viborg, the stature
of 65 very well-preserved adult skeletons (31 male, 34 female) were
determined (Boldsen 1984, Appendix 1). Based on these stature estimates,
four regression functions are presented: separated by gender for Fem1
and Tib1. Due to almost identical slopes, these functions are aggregated
according to bone type and separated only by the intercept for modern
populations (Danes, Finns) and the White Americans data from Trotter &
Gleser (1958). Here, we use the formulas obtained from the primary
archaeological data, the results of which are averaged when both bone
measurements are available.

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
  right) + 1 Tib1

## Usage

``` r
boldsen_1984(df)
```

## Arguments

- df:

  data.frame of type statuaar_data_table, containing informations on
  individual, bone and measurement.

## Value

data.frame with calculated stature and related information per
individual.

## References

Boldsen J (1984). “A statistical evaluation of the basis for predicting
stature from lengths of long bones in European populations.” *American
Journal of Physical Anthropology*, **65**(3), 305–311. ISSN 1096-8644,
[doi:10.1002/ajpa.1330650310](https://doi.org/10.1002/ajpa.1330650310) ,
<https://onlinelibrary.wiley.com/doi/abs/10.1002/ajpa.1330650310>.

## Author

Christoph Rinne <crinne@ufg.uni-kiel.de>

## Examples

``` r
# Read example dataset into a data frame.
x <- statuAAR::TrotterGleser1952
x <- x[x$Race == "White", ]
#'
# Create & check the data frame of mesures concordance for Trotter & Gleser 1952
measures.concordance <- create.measures.concordance()
measures.concordance[measures.concordance$own != "",]
#>    short      long  own
#> 1   Fem1   Femur.1  Fem
#> 10  Hum1 Humerus.1  Hum
#> 19  Rad1  Radius.1  Rad
#> 28  Tib1   Tibia.1  Tib
#> 37  Uln1    Ulna.1 Ulna
#'
# Prepare statuaar_data_table
dl.tgw <- statuAAR::prep.statuaar.data(x, d.form = "wide", ind = "Appendix_row",
                             sex = "Sex", measures.names = "own", stats = FALSE)
#> Error in loadNamespace(x): there is no package called ‘tidyr’
#'
# Calculate stature estimation using a given formula.
statuAAR::getStature(c("bo84"), dl.tgw)
#> Error: object 'dl.tgw' not found
# boldsen_1984(dl.tgw) # The alternative.
```
