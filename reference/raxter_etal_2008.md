# Calculate stature estimation according to: Raxter el al 2008.

Stature estimation (mm) based on the hierarchy of different regression
calculations, separated by sex (Raxter el al 2008). Bone measures used
in hierarchical order of correlation (r): male: Fem1+Tib1a, Fem2+Tib1a,
Tib1a, Tib1b, Fem1, Fem2, Hum1+Rad1, Hum1, Rad1 female: Tib1b, Tib1a,
Fem2+Tib1b, Fem1+Tib1a, Fem2, Fem1, Hum1, Hum1+Rad1, Rad1 If bone
measures for left and right are provided the mean value will be used,
but for statistic information 2 bones will be counted (n_measures). If
sex is indet. the mean of male and female stature estimation is given.
The authors do not mention, if the mean of all regressions is used or
the first possible stature estimation according to hierarchical order of
correlation. As the authors discuss the correlation and refere to the
work of Trotter & Gleser a hierarchical order is supposed (s. Siegmund
2010, p. 20). As correlation within male and females is highly different
and provides a sex specific order of regressions the stature of
individuals with indeterminated sex (indet.) is calculated by the mean
of both at each rank mentioning all bone measures used and the sum of
n_measures.

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
raxter_etal_2008(df)
```

## Arguments

- df:

  data.frame of type statuaar_data_table, containing informations on
  individual, bone and measurement.

## Value

data.frame with calculated stature and related information per
individual.

## References

Raxter MH, Ruff CB, Azab A, Erfan M, Soliman M, El-Sawaf A (2008).
“Stature estimation in ancient Egyptians: A new technique based on
anatomical reconstruction of stature.” *American Journal of Physical
Anthropology*, **136**(2), 147–155. ISSN 1096-8644,
[doi:10.1002/ajpa.20790](https://doi.org/10.1002/ajpa.20790) .

Siegmund F (2010). *Die Körpergröße der Menschen in der Ur- und
Frühgeschichte Mitteleuropas und ein Vergleich ihrer anthropologischen
Schätzmethoden*. Books on Demand, Norderstedt. ISBN 978-3-8391-5314-7,
tex.ids: siegmundKorpergrosseMenschenUr2010a.

Trotter M, Gleser GC (1952). “Estimation of stature from long bones of
American Whites and Negroes.” *American Journal of Physical
Anthropology*, **10**(4), 463–514. ISSN 1096-8644,
[doi:10.1002/ajpa.1330100407](https://doi.org/10.1002/ajpa.1330100407) .

Trotter M, Gleser GC (1977). “Corrigenda to “estimation of stature from
long limb bones of American Whites and Negroes,” American Journal
Physical Anthropology (1952).” *American Journal of Physical
Anthropology*, **47**(2), 355–356. ISSN 1096-8644,
[doi:10.1002/ajpa.1330470216](https://doi.org/10.1002/ajpa.1330470216) .

## Author

Hendrik Raese <h.raese@ufg.uni-kiel.de>

Christoph Rinne <crinne@ufg.uni-kiel.de>

## Examples

``` r
# Read example dataset into a data frame
```
