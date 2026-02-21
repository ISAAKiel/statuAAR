# Calculate stature estimation according to: Ruff el al 2012, All (indet).

Stature estimation (mm) based on the hierarchy of different regression
calculations, separated by sex and region (Ruff et al 2012). The authors
provide various formulas, whereby they primarily use the sum of femur
and tibia length for the regional groups due to its greater accuracy,
while the undifferentiated calculations are based on separate
measurements of the femur, humerus and radius. Depending on the
measurements provided for each individual, regional assignment may
result in a missing stature estimate. In addition, as an alternative to
each separately calculated regional group, a calculation can be
performed for a mixed data set. The authors do not refer to a
hierarchical application of the respective formulas, but the repeated
emphasis on %SEE as a quality characteristic suggests this. In addition,
averaging the results of both formulas for the respective regional group
would inadmissibly include the length of the tibia multiple times. Bone
measures used in hierarchical order of percent standard error of
estimate (%SEE):

If bone measures for left and right are provided the mean value will be
used, but for statistic information 2 bones will be counted
(n_measures).

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
ruff_etal_2012_a(df)
```

## Arguments

- df:

  data.frame of type statuaar_data_table, containing informations on
  individual, bone and measurement.

## Value

data.frame with calculated stature and related information per
individual.

## References

Ruff CB, Holt BM, Niskanen M, Sladék V, Berner M, Garofalo E, Garvin HM,
Hora M, Maijanen H, Niinimäki S, Salo K, Schuplerová E, Tompkins D
(2012). “Stature and body mass estimation from skeletal remains in the
European Holocene.” *American Journal of Physical Anthropology*,
**148**(4), 601–617. ISSN 1096-8644,
[doi:10.1002/ajpa.22087](https://doi.org/10.1002/ajpa.22087) .

Ruff C (2018). *Skeletal variation and adaptation in Europeans: upper
Paleolithic to the Twentieth Century*. Wiley Blackwell, Hoboken, NJ.
ISBN 978-1-118-62843-0, <https://doi.org/10.1002/9781118628430.ch1>.

Ruff C (2018). *Skeletal variation and adaptation in Europeans: upper
Paleolithic to the Twentieth Century*. John Wiley & Sons, Hoboken, NJ.
ISBN 978-1-118-62796-9.

## Author

Christoph Rinne <crinne@ufg.uni-kiel.de>

## Examples

``` r
# Read example dataset into a data frame
```
