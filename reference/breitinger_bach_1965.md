# Calculate stature estimation according to Breitinger 1938 & Bach 1965.

Stature estimation (mm) based on the mean of different regression
calculations, separated by sex (Breitinger 1938, Bach 1965). Bone
measures used: Hum2, Hum1, Rad1b, Fem1, Tib1b.

If bone measures for left and right are provided the mean value will be
used, but for statistic information 2 bones will be counted
(n_measures). If sex is indet. the mean of male and female stature
estimation is given. Breitinger (1938) does not show the regression
equation for Hum1, but the equation can be derived from the values given
in the table p. 272 (see Siegmund 2010, p. 112). In contrast, Bach
(1965) gives both equations. In addition, Bach (1965, 20) states to use
only one of the Humeri measures for estimation (Hum1 or Hum2 or the mean
of both) to avoid inadmissable multiplication of the bone within the
stature estimation.

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
breitinger_bach_1965(df)
```

## Arguments

- df:

  data.frame of type statuaar_data_table, containing informations on
  individual, bone and measurement

## Value

data.frame, containing one data.frame with all calculated indices for
every individual

## References

Bach H (1965). “Zur Berechnung der Körperhöhe aus den langen
Gliedmaßenknochen weiblicher Skelette.” *Anthropologischer Anzeiger*,
**29**, 12–21. ISSN 0003-5548.

Breitinger E (1937). “Zur Berechnung der Körperhöhe aus den langen
Gliedmaßenknochen.” *Anthropologischer Anzeiger*, **14**(3/4), 249–274.
ISSN 0003-5548.

Siegmund F (2010). *Die Körpergröße der Menschen in der Ur- und
Frühgeschichte Mitteleuropas und ein Vergleich ihrer anthropologischen
Schätzmethoden*. Books on Demand, Norderstedt. ISBN 978-3-8391-5314-7,
tex.ids: siegmundKorpergrosseMenschenUr2010a.

## Author

Christoph Rinne <crinne@ufg.uni-kiel.de>

## Examples

``` r
# Read example dataset into a data frame
x <- Bach1965
```
