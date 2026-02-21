# Calculate stature estimation according to: Telkkã 1950.

Stature estimation (mm) based on the mean of different regression
calculations, separated by sex. Bone measures used: Hum1, Rad2, Uln2,
Fem1, Tib1, Fib1

If bone measures for left and right are provided the mean value will be
used, but for statistic information 2 bones will be counted
(n_measures). If sex is indet. the mean of male and female stature
estimation is given. To retrieve the estimated stature 20 mm will be
substracted from the resulting mean value.

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
telkkae_1950(df)
```

## Arguments

- df:

  data.frame of type statuaar_data_table, containing informations on
  individual, bone and measurement.

## Value

data.frame with calculated stature and related information per
individual.

## References

Telkkã A (1950). “On the prediction of human stature from the long
bones.” *Acta Anatomica*, **9**, 103–117.

## Author

Christoph Rinne <crinne@ufg.uni-kiel.de>

## Examples

``` r
# Read example dataset into a data frame
```
