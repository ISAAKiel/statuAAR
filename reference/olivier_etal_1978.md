# Calculate stature estimation according to: Olivier et al 1978.

Stature estimation (mm) based on the hierarchy of different regression
calculations, separated by sex (Olivier et al 1978). Bone measures used:
Hum2, Hum1, Rad1b, Fem1, Tib1b

If bone measures for left and right are provided the mean value will be
used, but for statistic information 2 bones will be counted
(n_measures). If sex is indet. the mean of male and female stature
estimation is given. The regression formula are hierarchical from
combinations of different bone measures to single bone measures. For the
multiple regressions the average of left and right bone is used,
allthough "asymmetry to be virtually negligible: it is only significant
for the right ulna" (Olivier et al 1978, 515). For individual bone
measures regression formula are given for left an right side in the case
of male individuals. Wheras regression formula for females are presented
only for left bones, likely due to the set of data based on. In
consequence for male individuals stature estimation on a single bone is
calculated 1. upon the hierarchy given by the table (r, s.d.) and 2. on
the mean of left an right bone or 3. on one of both sides. Wheras for
female individuals stature estimation based on one bone considers

1.  the hierarchy given by the table (r, s.d) using the mean of left
    and/or right side. For male 15 multiple regressions are provided,
    for female only 9, in addition these use different bone measures
    within each hierarchy. In consequence, stature estimation for both
    do not correlate and the alternative stature will not be calculated.
    Only the first applicable measure of the given hierarchy will be
    used.

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
olivier_etal_1978(df)
```

## Arguments

- df:

  data.frame of type statuaar_data_table, containing informations on
  individual, bone and measurement.

## Value

data.frame with calculated stature and related information per
individual.

## References

Olivier G, Aaron C, Fully G, Tissier G (1978). “New estimations of
stature and cranial capacity in modern man.” *Journal of Human
Evolution*, **7**(6), 513–518. ISSN 0047-2484,
[doi:10.1016/S0047-2484(78)80020-7](https://doi.org/10.1016/S0047-2484%2878%2980020-7)
.

Olivier G, Tissier H (1975). “Estimation de la stature féminine d’après
les os longs des membres.” *Bulletins et Mémoires de la Société
d’Anthropologie de Paris*, **2**(4), 297–305.
[doi:10.3406/bmsap.1975.1821](https://doi.org/10.3406/bmsap.1975.1821) .

## Author

Hendrik Raese <h.raese@ufg.uni-kiel.de>

Christoph Rinne <crinne@ufg.uni-kiel.de>

## Examples

``` r
# Read example dataset into a data frame
```
