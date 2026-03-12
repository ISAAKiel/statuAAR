# Calculate stature estimation according to: Sjøvold 1990.

Stature estimation (mm) based on the mean of different regression
calculations, not separated by sex (Sjøvold 1990).

Bone measures used: Hum1, Rad1 (alt. Rad1b), Uln1, Fem1 (alt. Fem2),
Fib1.

If bone measures for left and right are provided the mean value will be
used, but for statistic information 2 bones will be counted
(n_measures). The author discusses the correlation and disadvantages of
the various regression calculations. Only the calculations based on the
tibia are rejected by Sjøvold, while all other regressions appear
equally valid to him (Sjøvold 1990, 444). Allthough not explained in the
text estimated stature is derived from the mean of all calculations from
the bone measures excluding Tibia. To not multiply significance in the
case of two measures per bone (e.g. Fem 1, Fem2) only one of both will
be calculated.

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
sjovold_1990(df)
```

## Arguments

- df:

  data.frame of type statuaar_data_table, containing informations on
  individual, bone and measurement.

## Value

data.frame with calculated stature and related information per
individual.

## References

Sjøvold T (1990). “Estimation of stature from long bones utilizing the
line of organic correlation.” *Human Evolution*, **5**(5), 431–447. ISSN
1824-310X, [doi:10.1007/BF02435593](https://doi.org/10.1007/BF02435593)
.

Ruff CB, Holt BM, Niskanen M, Sladék V, Berner M, Garofalo E, Garvin HM,
Hora M, Maijanen H, Niinimäki S, Salo K, Schuplerová E, Tompkins D
(2012). “Stature and body mass estimation from skeletal remains in the
European Holocene.” *American Journal of Physical Anthropology*,
**148**(4), 601–617. ISSN 1096-8644,
[doi:10.1002/ajpa.22087](https://doi.org/10.1002/ajpa.22087) .

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
sj90.estimates <- statuAAR::getStature(c("sj90"), dl.bach1965)
#> Error: object 'dl.bach1965' not found

# Extract the corresponding data frame from the returned list object.
sj90.estimates[["sj90"]]
#> Error: object 'sj90.estimates' not found
```
