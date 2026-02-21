# statuAAR data preparation and check functions

Human stature estimation is based on various measures of different
bones. A multitude of formula have been developed and provide a wide
range of possible results. The quality of the result depends mainly on
the representativity of the original sample of the formula with respect
to the currrent study. Thus a easy and fast forward calculation
according to various formula can be used for a camparison of the
results.

All measures have to be given in millimeters (mm). The measures used are
defined by R. Martin (1928). The labels of the measures of the data
aquisition may differ from those used by statuAAR. Therefore a
concordance of labels should be edited.

In addition summarised statistics for each measure across the dataset is
provided to check for data inconsitancy befor calculation.

- `create.measures.concordance`: Creates a data frame with three
  columns: **short** names (e.g. Hum1al), **long** names (e.g.
  Humerus.1a.left) and **own** to be filled with user defined names.

- `measures.statistics`: Calculates basic descriptive statistics to
  check data consitancy.

- `prep.statuaar.data`: Checks the input data: uniqueness of individual
  identifyer, accepted values for Sex and accepted measures names.
  Provides a data.frame of standardised measurements for calculation of
  body stature estimation with four columns: **Ind**(ividual), **Sex**,
  **variable** (mearsure name) and **value** (measured value).

## Usage

``` r
create.measures.concordance()

measures.statistics(dl)

prep.statuaar.data(
  x,
  d.form = "wide",
  ind = NA,
  sex = NA,
  measures.names = "short",
  stats = TRUE
)
```

## Arguments

- dl:

  statuAAR data list as provided by prep.statuaar.data.

- x:

  A simple data.frame containing the measurements per individual.

- d.form:

  A string defining the data.frame structure.

  - d.form=`wide` for a data.frame with individuals (rows) and
    measurements (columns).

  - d.form=`long` for a data.frame with one measure per individual in
    one row. With only two columns: `variable`(character, measure name
    e.g. hum1), `value` (numeric, length (mm)), each row represents one
    individual with only one measurement and is identified by the row
    number.

- ind:

  A string defining the column with identifiers for each individual. If
  ind = NA a column `Ind` with rownumbers will be added.

- sex:

  A string defining the column identifying the sex. If sex = NA a column
  `Sex` with `indet` will be added.

- measures.names:

  A string defining the set of predefined or own measure names used. For
  `own` a data.frame `measures.concordance` for correlation (merge) is
  needed.

  - measures=`short`: Bone (3 letters), measure acc. to Martin (1928),
    laterality (1 letter) without any separation (e.g. Hum1, Hum1l,
    Hum1r, Hum1a, Hum1al, Hum1ar etc.).

  - measures=`long`: Bone measure acc. to Martin (1928), laterality
    separated by `.` (e.g. Humerus.1, Humerus.1.left, Humerus.1a.left,
    etc.).

  - measures=`own`: A data.frame `measures.concordance` with own names
    to be merged is needed.

- stats:

  Output of aggregating statistics of the measures provided. Default =
  TRUE.

## Value

A list with a dataframe for each formula selected.

- **Ind**: identifyer for each individual.

- **Sex**: sex of the individual. Accepted values 1 (male), 2 (female),
  3 (indet) or `m`(ale), `f`(emale), `indet`.

- **variable**: short name of the measure

- **value**: measured value.

## References

Martin R (1928). *Lehrbuch der Anthropologie in systematischer
Darstellung. Mit besonderer Berücksichtigung der anthropologischen
Methoden. Band 2: Kraniologie, Osteologie.*, 2. edition. G. Fischer,
Jena.
<http://digital.zbmed.de/physische_anthropologie/content/titleinfo/555255>.

## Author

Christoph Rinne <crinne@ufg.uni-kiel.de>

Hendrik Raese <h.raese@ufg.uni-kiel.de>

## Examples

``` r
# Read example dataset into a data frame

# Read example dataset into a data frame
x <- TrotterGleser1952
# If not yet existent create a list of measure names to be used
# measures.concordance <- create.measures.concordance()
# Edit the measures.concordance (not needed for this dataset)
# measures.concordance$own[measures.concordance$short=="Fem1"]<-"Fem"

# get a dataframe with measures to process
dl.trotter.gleser <- prep.statuaar.data(x, d.form = "wide",
   ind = "Appendix_row", sex = "Sex", measures.names = "own", stats = FALSE)
# See basic statistics to check for errors
measures.statistics(dl.trotter.gleser)
#> # A tibble: 5 × 8
#>   measure     n  MinM Quart1 MedianM MeanM Quart3  MaxM
#>   <chr>   <dbl> <dbl>  <dbl>   <dbl> <dbl>  <dbl> <dbl>
#> 1 Fem1      184   348    414     464   465    514   605
#> 2 Hum1      184   244    291     328   328    364   417
#> 3 Rad1      184   165    217     248   250    282   340
#> 4 Tib1      184   271    328     374   375    418   511
#> 5 Uln1      184   193    235     266   268    298   364

# For the data from Rollet 1888
rollet1888 <- Rollet1888
# 1. Create an identifyer due to identical numbering of females and males
rollet1888$id<-paste(rollet1888$Sex, rollet1888$Nr, sep="_")
# 2. Fill in the mesasures names in the column "own" of the measures.concordance
measures.concordance <- measures.concordance.rollet1888
# 3. Read the data
dl.rollet1888 <- prep.statuaar.data(rollet1888, d.form = "wide",
      ind="id", sex = "Sex", measures.names = "own")
#> # A tibble: 1 × 8
#>   measure     n  MinM Quart1 MedianM MeanM Quart3  MaxM
#>   <chr>   <dbl> <dbl>  <dbl>   <dbl> <dbl>  <dbl> <dbl>
#> 1 NA          0    NA     NA      NA   NaN     NA    NA
```
