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
bone measures. Only the first applicable measure will be used (Siegmund
2010, 112).

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

Siegmund F (2010). *Die Körpergröße der Menschen in der Ur- und
Frühgeschichte Mitteleuropas und ein Vergleich ihrer anthropologischen
Schätzmethoden*. Books on Demand, Norderstedt. ISBN 978-3-8391-5314-7,
tex.ids: siegmundKorpergrosseMenschenUr2010a.

## Author

Hendrik Raese <h.raese@ufg.uni-kiel.de>

Christoph Rinne <crinne@ufg.uni-kiel.de>

## Examples

``` r
# Read example dataset into a data frame.
x <- statuAAR::TrotterGleser1952
x <- x[x$Race == "Black", ]
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
dl.tgb <- statuAAR::prep.statuaar.data(x, d.form = "wide", ind = "Appendix_row",
                                       sex = "Sex", measures.names = "own", stats = FALSE)
#> Error in loadNamespace(x): there is no package called ‘tidyr’
#'
# Calculate stature estimation using a given formula.
statuAAR::getStature(c("tg01"), dl.tgb)
#> Error: object 'dl.tgb' not found
# trotter_gleser_1952_an(dl.tgb) # The alternative.
```
