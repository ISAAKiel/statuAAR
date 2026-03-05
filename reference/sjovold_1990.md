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

# Calculate stature estimation using a given formula.
sj90.estimates <- statuAAR::getStature(c("sj90"), dl.bach1965)

# Extract the corresponding data frame from the returned list object.
sj90.estimates[["sj90"]]
#>    sex stature              bone if_female if_male if_indet n_measures
#> 32   m    1436 Hum1, Rad1b, Fem1      1436    1436     1436          3
#> 33   m    1452 Hum1, Rad1b, Fem1      1452    1452     1452          3
#> 34   m    1469 Hum1, Rad1b, Fem1      1469    1469     1469          3
#> 35   m    1485 Hum1, Rad1b, Fem1      1485    1485     1485          3
#> 36   m    1501 Hum1, Rad1b, Fem1      1501    1501     1501          3
#> 37   m    1519 Hum1, Rad1b, Fem1      1519    1519     1519          3
#> 38   m    1536 Hum1, Rad1b, Fem1      1536    1536     1536          3
#> 39   m    1551 Hum1, Rad1b, Fem1      1551    1551     1551          3
#> 40   m    1570 Hum1, Rad1b, Fem1      1570    1570     1570          3
#> 41   m    1586 Hum1, Rad1b, Fem1      1586    1586     1586          3
#> 42   m    1601 Hum1, Rad1b, Fem1      1601    1601     1601          3
#> 43   m    1619 Hum1, Rad1b, Fem1      1619    1619     1619          3
#> 44   m    1635 Hum1, Rad1b, Fem1      1635    1635     1635          3
#> 45   m    1652 Hum1, Rad1b, Fem1      1652    1652     1652          3
#> 46   m    1668 Hum1, Rad1b, Fem1      1668    1668     1668          3
#> 47   m    1684 Hum1, Rad1b, Fem1      1684    1684     1684          3
#> 48   m    1702 Hum1, Rad1b, Fem1      1702    1702     1702          3
#> 49   m    1717 Hum1, Rad1b, Fem1      1717    1717     1717          3
#> 50   m    1734 Hum1, Rad1b, Fem1      1734    1734     1734          3
#> 51   m    1750 Hum1, Rad1b, Fem1      1750    1750     1750          3
#> 52   m    1767 Hum1, Rad1b, Fem1      1767    1767     1767          3
#> 53   m    1784 Hum1, Rad1b, Fem1      1784    1784     1784          3
#> 54   m    1800 Hum1, Rad1b, Fem1      1800    1800     1800          3
#> 55   m    1816 Hum1, Rad1b, Fem1      1816    1816     1816          3
#> 56   m    1834 Hum1, Rad1b, Fem1      1834    1834     1834          3
#> 57   m    1851 Hum1, Rad1b, Fem1      1851    1851     1851          3
#> 58   m    1866 Hum1, Rad1b, Fem1      1866    1866     1866          3
#> 59   m    1884 Hum1, Rad1b, Fem1      1884    1884     1884          3
#> 60   m    1900 Hum1, Rad1b, Fem1      1900    1900     1900          3
#> 61   m    1915 Hum1, Rad1b, Fem1      1915    1915     1915          3
#> 62   m    1933 Hum1, Rad1b, Fem1      1933    1933     1933          3
#> 63   m    1949 Hum1, Rad1b, Fem1      1949    1949     1949          3
#> 64   m    1967 Hum1, Rad1b, Fem1      1967    1967     1967          3
#> 1    f    1224 Hum1, Rad1b, Fem1      1224    1224     1224          3
#> 10   f    1426 Hum1, Rad1b, Fem1      1426    1426     1426          3
#> 11   f    1448 Hum1, Rad1b, Fem1      1448    1448     1448          3
#> 12   f    1470 Hum1, Rad1b, Fem1      1470    1470     1470          3
#> 13   f    1492 Hum1, Rad1b, Fem1      1492    1492     1492          3
#> 14   f    1515 Hum1, Rad1b, Fem1      1515    1515     1515          3
#> 15   f    1538 Hum1, Rad1b, Fem1      1538    1538     1538          3
#> 16   f    1560 Hum1, Rad1b, Fem1      1560    1560     1560          3
#> 17   f    1583 Hum1, Rad1b, Fem1      1583    1583     1583          3
#> 18   f    1605 Hum1, Rad1b, Fem1      1605    1605     1605          3
#> 19   f    1628 Hum1, Rad1b, Fem1      1628    1628     1628          3
#> 2    f    1245 Hum1, Rad1b, Fem1      1245    1245     1245          3
#> 20   f    1650 Hum1, Rad1b, Fem1      1650    1650     1650          3
#> 21   f    1673 Hum1, Rad1b, Fem1      1673    1673     1673          3
#> 22   f    1696 Hum1, Rad1b, Fem1      1696    1696     1696          3
#> 23   f    1716 Hum1, Rad1b, Fem1      1716    1716     1716          3
#> 24   f    1739 Hum1, Rad1b, Fem1      1739    1739     1739          3
#> 25   f    1763 Hum1, Rad1b, Fem1      1763    1763     1763          3
#> 26   f    1786 Hum1, Rad1b, Fem1      1786    1786     1786          3
#> 27   f    1807 Hum1, Rad1b, Fem1      1807    1807     1807          3
#> 28   f    1829 Hum1, Rad1b, Fem1      1829    1829     1829          3
#> 29   f    1852 Hum1, Rad1b, Fem1      1852    1852     1852          3
#> 3    f    1268 Hum1, Rad1b, Fem1      1268    1268     1268          3
#> 30   f    1875 Hum1, Rad1b, Fem1      1875    1875     1875          3
#> 31   f    1897 Hum1, Rad1b, Fem1      1897    1897     1897          3
#> 4    f    1291 Hum1, Rad1b, Fem1      1291    1291     1291          3
#> 5    f    1314 Hum1, Rad1b, Fem1      1314    1314     1314          3
#> 6    f    1336 Hum1, Rad1b, Fem1      1336    1336     1336          3
#> 7    f    1358 Hum1, Rad1b, Fem1      1358    1358     1358          3
#> 8    f    1381 Hum1, Rad1b, Fem1      1381    1381     1381          3
#> 9    f    1402 Hum1, Rad1b, Fem1      1402    1402     1402          3
```
