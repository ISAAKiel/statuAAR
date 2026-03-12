# Calculate stature estimation according to: Pearson 1899

Stature estimation (mm) based on the mean of several multiple and single
regression calculations, separated by sex (Pearson 1899, p. 196). Bone
measures used: Hum1, Rad1, Fem1, Tib1b, Tib1a

If bone measures for left and right are provided the mean value will be
used, but for statistic information 2 bones will be counted
(n_measures). If sex is indet. the mean of male and female stature
estimation is given. According to Pearson (1899, 196-97) following
measure substitudes will be used:

- male: Fem1 = Fem2 + 3.2 mm = Fem1.corr, Tib1b = Tib1a - 9.6 mm =
  Tib1a.corr

- female: Fem1 = Fem2 + 3.3 mm = Fem1.corr, Tib1b = Tib1a - 8.7 mm =
  Tib1a.corr

Pearson's work is based on the bones from the right side. According to
measurements from Rollet (1888) he provides correction values for the
bones of the left side. These values vary for male within +/- .4 to 4.2
mm and for female within +/- 0.4 to 5.1 mm. Leg length discrepancy (LLD)
is a complex process due to multiple causes with environmental and
genetic factors and increasing with age. Based on French data minor LLD
is often, but if treated in most cases below 2 cm per leg. In addition
differences between sex and left vs right side are observed (Guichet et
al 1991, Holliday & Ruff 2001, Knutson 2005). In consequence a
compounded correction between left and right bones by the values derived
from the sample of Rollet (1888) is rejected.

Pearson applies his formula to several data sets ("races") and allways
caculates the mean value of all applicable formulas (1988, pp. 198,
205). Accordingly, all possible estimates are calculated for each
individual and then averaged. This can lead to an overrepresentation of
individual bone measurements.

Since he does not provide formulas for individuals of indeterminate sex,
the results for female and male are averaged here.

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
pearson_1899(df)
```

## Arguments

- df:

  data.frame of type statuaar_data_table, containing informations on
  individual, bone and measurement.

## Value

data.frame with calculated stature and related information per
individual.

## References

Pearson K (1899). “IV. Mathematical contributions to the theory of
evolution.—V. On the reconstruction of the stature of prehistoric
races.” *Philosophical Transactions of the Royal Society of London.
Series A, Containing Papers of a Mathematical or Physical Character*,
**192**, 169–244.
[doi:10.1098/rsta.1899.0004](https://doi.org/10.1098/rsta.1899.0004) .
Rollet E (1888). *De la mensuration des os longs des membres dans ses
rapports avec l’anthropologie, la clinique et la médicine judicaire*. A.
Storck, Lyon.
<https://archive.org/details/BIUSante_57377/page/n11/mode/2up>. Guichet
J, Spivak JM, Trouilloud P, Grammont PM (1991). “Lower Limb-Length
Discrepancy: An Epidemiologic Study.” *Clinical Orthopaedics and Related
Research®*, **272**, 235–241. ISSN 0009-921X. Holliday TW, Ruff CB
(2001). “Relative variation in human proximal and distal limb segment
lengths.” *American Journal of Physical Anthropology*, **116**(1),
26–33. ISSN 1096-8644,
[doi:10.1002/ajpa.1098](https://doi.org/10.1002/ajpa.1098) . Knutson GA
(2005). “Anatomic and functional leg-length inequality: A review and
recommendation for clinical decision-making. Part I, anatomic leg-length
inequality: prevalence, magnitude, effects and clinical significance.”
*Chiropractic & Osteopathy*, **13**, 11. ISSN 1746-1340,
[doi:10.1186/1746-1340-13-11](https://doi.org/10.1186/1746-1340-13-11) .

## Author

Christoph Rinne <crinne@ufg.uni-kiel.de>

## Examples

``` r
# Read example dataset Neanderthal Man (Pearson 1899, p. 205).
x <- data.frame(Fem1 = 445.2, Hum1 = 312, Rad1 = 240)
dl <- statuAAR::prep.statuaar.data(x, d.form = "wide", stat = FALSE)
#> Warning: No individual identifier provided, each record (row) will be counted as one individual.
#> Error in loadNamespace(x): there is no package called ‘tidyr’
pearson_1899(dl)
#> Error: object 'dl' not found
# The result differs from Pearson's: 162.96 cm. He uses the formulas for men,
# does not calculate his formula (g), even though it is applicable, and the
# results for formulas (d), (h) and (i) differ by up to 19 mm from the
# corresponding individual results in statuAAR.
```
