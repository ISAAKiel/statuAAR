# Calculate stature estimation according to: Feldesman et al 1990.

Stature estimation (mm) for Homo erectus (HE), early Neanderthal (EN),
Near Eastern Neanderthal (NEN), and early anatomically modern Homo
sapiens (EAMHS) based on a regression calculation of one bone
measurement, not separated by sex (Feldesman et al 1990). Bone measures
used: Fem1

If bone measures for left and right are provided the mean value will be
used, but for statistic information 2 bones will be counted
(n_measures). If sex is indet. the mean of male and female stature
estimation is given. Based on the measurement of the Femur (Fem1) of
different individuals, the stature for males and females is calculated.

Returns a data.frame with:

- ind: individual identifyer (rownames),

- sex: as provided for calculation: m, f, indet.

- stature: estimated on the provided sex and bone measures,

- bone (measure(s)): bones used for calculation,

- if_female (stature): columns with alternative stature for three sex
  classes,

- if_male (stature),

- if_indet. (stature) and

- n_measures: number of bone measures included: e.g. 2 Fem1 (left,
  right)

## Usage

``` r
feldesman_etal_1990(df)
```

## Arguments

- df:

  data.frame of type statuaar_data_table, containing informations on
  individual, bone and measurement.

## Value

data.frame with calculated stature and related information per
individual.

## References

Feldesman MR, Kleckner JG, Lundy JK (1990). “Femur/stature ratio and
estimates of stature in mid- and late-pleistocene fossil hominids.”
*American Journal of Physical Anthropology*, **83**(3), 359–372. ISSN
1096-8644,
[doi:10.1002/ajpa.1330830309](https://doi.org/10.1002/ajpa.1330830309) .

## Author

Christoph Rinne <crinne@ufg.uni-kiel.de>

## Examples

``` r
# Read example dataset into a data frame
x <- statuAAR::Feldesman1990
# Check for one individual per site
table(table(x$Site))
#> 
#>  1 
#> 48 

# Reduce gender, e.g. M(a), information to m, f.
x$Probable.gender <- tolower(gsub("^(\\w).*", "\\1", x$Probable.gender))
x$Probable.gender[nchar(x$Probable.gender) == 0] <- "i"

# If not yet existent create a list of measure names to be used
measures.concordance <- create.measures.concordance()
# Edit the measures.concordance for this dataset
measures.concordance$own[measures.concordance$short == "Fem1"] <- "Femur.length"

# Prepare tabled data into a long list (statuaar_data_table)
dl.fe90 <- statuAAR::prep.statuaar.data(x, d.form = "wide", ind = "Site",
              measures.names = "own", sex = "Probable.gender", stats = FALSE)
#> Error in loadNamespace(x): there is no package called ‘tidyr’

# Calculate stature estimation using this formula.
fe90.estimates <- statuAAR::getStature(c("fe90"), dl.fe90)
#> Error: object 'dl.fe90' not found

# Extract the corresponding data frame from the returned list object.
fe90.estimates[["fe90"]]
#> Error: object 'fe90.estimates' not found
```
