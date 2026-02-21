# Calculate stature estimation according to: Sjøvold 1990.

Stature estimation (mm) based on the mean of different regression
calculations, not separated by sex (Sjøvold 1990). Bone measures used:
Hum1, Rad1 (alt. Rad1b), Uln1, Fem1 (alt. Fem2), Fib1. If bone measures
for left and right are provided the mean value will be used, but for
statistic information 2 bones will be counted (n_measures). The author
discusses the correlation and disadvantages of the various regression
calculations. Only the calculations based on the tibia are rejected by
Sjøvold, while all other regressions appear equally valid to him. Ruff
et al (2012) recognise a known trend towards geographical latitude for
the tibia. Allthough not explained in the text estimated stature is
derived from the mean of all calculations from the bone measures
excluding Tibia (s.a.). To not multiply significance in the case of two
measures per bone (e.g. Fem 1, Fem2) only one of both will be
calculated.

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
```
