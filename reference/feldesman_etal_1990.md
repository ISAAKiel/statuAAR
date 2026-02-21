# Calculate stature estimation according to: Feldesman et al 1990.

Stature estimation (mm) based on a regression calculation of one bone,
not separated by sex (Feldesman et al 1990). Bone measures used: Fem1

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

- female (stature): columns with alternative stature for three sex
  classes,

- male (stature),

- indet. (stature) and

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
```
