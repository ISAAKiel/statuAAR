# Calculate stature estimation based on bone measures according to: Formicola & Franceschi 1996.

Stature estimation (mm) based on a hierarchical order of regression
calculations, separated by sex and based on: Fem2 + Tib1, Fem1, Tib1,
Hum1, Rad1. If bone measures for left and right are provided the mean
value will be used, but for statistic information 2 bones will be
counted (n_measures). If sex is indet. the mean of male and female
stature estimation is given. Formicola and Franceschi (1996) do not
provide information on hierarchie or mean calculation of the regression
formula, but table 3 gives standard error (S.E.) and correlation from
which a hierarchical order can be derived. The order of bone
measurements is identical for men and women for the first three
formulas, although the respective SE differ significantly: Fem2 + Tib1,
Fem1, Tib1. For women, Hum1 and Rad1 follow with a significant
difference in SE, while for men, with a small SE difference, Rad1
follows first and then Hum1 (1996, Tab. 3). For individuals of
indeterminate sex, stature estimatation is only possible by disregarding
the hierarchical order or by taking the mean of two different bone
measurements. For this reason, the hierarchical order of formulas for
women is used for all individuals. Due to this, the order used differs
slightly from Siegmund (2010, 116 (12.8)).

Bone measures used: Fem2 + Tib1 (both present), Tib1, Fem1, Hum1, Rad1
(or Rad1a)

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
  right), 1 Tib1

## Usage

``` r
formicola_franceschi_1996(df)
```

## Arguments

- df:

  data.frame of type statuaar_data_table, containing informations on
  individual, bone and measurement.

## Value

data.frame with calculated stature and related information per
individual.

## References

Formicola V, Franceschi M (1996). “Regression equations for estimating
stature from long bones of early Holocene European samples.” *American
Journal of Physical Anthropology*, **100**(1), 83–88. ISSN 1096-8644,
[doi:10.1002/(SICI)1096-8644(199605)100:1\\3C83::AID-AJPA8\\3E3.0.CO;2-E](https://doi.org/10.1002/%28SICI%291096-8644%28199605%29100%3A1%5C%253C83%3A%3AAID-AJPA8%5C%253E3.0.CO%3B2-E)
.

Formicola V (1993). “Stature reconstruction from long bones in ancient
population samples: An approach to the problem of its reliability.”
*American Journal of Physical Anthropology*, **90**(3), 351–358. ISSN
1096-8644,
[doi:10.1002/ajpa.1330900309](https://doi.org/10.1002/ajpa.1330900309) .

Siegmund F (2010). *Die Körpergröße der Menschen in der Ur- und
Frühgeschichte Mitteleuropas und ein Vergleich ihrer anthropologischen
Schätzmethoden*. Books on Demand, Norderstedt. ISBN 978-3-8391-5314-7,
tex.ids: siegmundKorpergrosseMenschenUr2010a.

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
ff96.estimates <- statuAAR::getStature(c("ff96"), dl.bach1965)

# Extract the corresponding data frame from the returned list object.
ff96.estimates[["ff96"]]
#>    sex stature    bone if_female if_male if_indet n_measures
#> 32   m    1446 2. Fem1      1408    1446     1427          1
#> 33   m    1462 2. Fem1      1424    1462     1443          1
#> 34   m    1477 2. Fem1      1439    1477     1458          1
#> 35   m    1492 2. Fem1      1455    1492     1474          1
#> 36   m    1508 2. Fem1      1471    1508     1489          1
#> 37   m    1523 2. Fem1      1486    1523     1505          1
#> 38   m    1538 2. Fem1      1502    1538     1520          1
#> 39   m    1554 2. Fem1      1518    1554     1536          1
#> 40   m    1571 2. Fem1      1536    1571     1554          1
#> 41   m    1587 2. Fem1      1551    1587     1569          1
#> 42   m    1602 2. Fem1      1567    1602     1585          1
#> 43   m    1617 2. Fem1      1583    1617     1600          1
#> 44   m    1633 2. Fem1      1598    1633     1616          1
#> 45   m    1648 2. Fem1      1614    1648     1631          1
#> 46   m    1663 2. Fem1      1630    1663     1646          1
#> 47   m    1678 2. Fem1      1645    1678     1662          1
#> 48   m    1694 2. Fem1      1661    1694     1677          1
#> 49   m    1709 2. Fem1      1677    1709     1693          1
#> 50   m    1724 2. Fem1      1692    1724     1708          1
#> 51   m    1740 2. Fem1      1708    1740     1724          1
#> 52   m    1758 2. Fem1      1726    1758     1742          1
#> 53   m    1773 2. Fem1      1742    1773     1757          1
#> 54   m    1788 2. Fem1      1758    1788     1773          1
#> 55   m    1803 2. Fem1      1773    1803     1788          1
#> 56   m    1819 2. Fem1      1789    1819     1804          1
#> 57   m    1834 2. Fem1      1805    1834     1819          1
#> 58   m    1849 2. Fem1      1820    1849     1835          1
#> 59   m    1865 2. Fem1      1836    1865     1850          1
#> 60   m    1880 2. Fem1      1852    1880     1866          1
#> 61   m    1895 2. Fem1      1867    1895     1881          1
#> 62   m    1911 2. Fem1      1883    1911     1897          1
#> 63   m    1926 2. Fem1      1899    1926     1912          1
#> 64   m    1941 2. Fem1      1914    1941     1928          1
#> 1    f    1223 2. Fem1      1223    1265     1244          1
#> 10   f    1400 2. Fem1      1400    1439     1419          1
#> 11   f    1421 2. Fem1      1421    1459     1440          1
#> 12   f    1439 2. Fem1      1439    1477     1458          1
#> 13   f    1460 2. Fem1      1460    1497     1479          1
#> 14   f    1481 2. Fem1      1481    1518     1499          1
#> 15   f    1499 2. Fem1      1499    1536     1517          1
#> 16   f    1520 2. Fem1      1520    1556     1538          1
#> 17   f    1541 2. Fem1      1541    1576     1559          1
#> 18   f    1559 2. Fem1      1559    1594     1577          1
#> 19   f    1580 2. Fem1      1580    1615     1597          1
#> 2    f    1241 2. Fem1      1241    1283     1262          1
#> 20   f    1598 2. Fem1      1598    1633     1616          1
#> 21   f    1619 2. Fem1      1619    1653     1636          1
#> 22   f    1640 2. Fem1      1640    1673     1657          1
#> 23   f    1658 2. Fem1      1658    1691     1675          1
#> 24   f    1679 2. Fem1      1679    1712     1696          1
#> 25   f    1698 2. Fem1      1698    1729     1714          1
#> 26   f    1719 2. Fem1      1719    1750     1734          1
#> 27   f    1739 2. Fem1      1739    1770     1755          1
#> 28   f    1758 2. Fem1      1758    1788     1773          1
#> 29   f    1779 2. Fem1      1779    1809     1794          1
#> 3    f    1262 2. Fem1      1262    1304     1283          1
#> 30   f    1799 2. Fem1      1799    1829     1814          1
#> 31   f    1818 2. Fem1      1818    1847     1832          1
#> 4    f    1283 2. Fem1      1283    1324     1303          1
#> 5    f    1301 2. Fem1      1301    1342     1321          1
#> 6    f    1322 2. Fem1      1322    1362     1342          1
#> 7    f    1340 2. Fem1      1340    1380     1360          1
#> 8    f    1361 2. Fem1      1361    1401     1381          1
#> 9    f    1382 2. Fem1      1382    1421     1401          1
```
