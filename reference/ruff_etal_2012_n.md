# Calculate stature estimation according to: Ruff el al 2012, North.

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
would inadmissibly include the length of the tibia multiple times.

Bone measures (Fem1+Tib1, Tib1) used in hierarchical order of percent
standard error of estimate (%SEE). The addition of individual bone
measurements is performed during the calculation.

If bone measures for left and right are provided the mean value will be
used, but for statistic information 2 bones will be counted
(n_measures).

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
ruff_etal_2012_n(df)
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
# Read example dataset into a data frame.
# Ruff et al 2012 cite Trotter & Gleser (White).
x <- statuAAR::TrotterGleser1952
x <- x[x$Race == "White", ]

# Create & check the data frame of mesures concordance for Trotter & Gleser 1952
measures.concordance <- create.measures.concordance()
measures.concordance[measures.concordance$own != "",]
#>    short      long  own
#> 1   Fem1   Femur.1  Fem
#> 10  Hum1 Humerus.1  Hum
#> 19  Rad1  Radius.1  Rad
#> 28  Tib1   Tibia.1  Tib
#> 37  Uln1    Ulna.1 Ulna

# Prepare statuaar_data_table
dl.tgb <- statuAAR::prep.statuaar.data(x, d.form = "wide", ind = "Appendix_row",
                           sex = "Sex", measures.names = "own", stats = FALSE)

# Calculate stature estimation using a given formula.
statuAAR::getStature(c("r12n"), dl.tgb)
#> $r12n
#>      sex stature      bone if_female if_male if_indet n_measures
#> 1_1    m    1437 Fem1+Tib1      1440    1437     1437          2
#> 1_10   m    1547 Fem1+Tib1      1545    1547     1547          2
#> 1_11   m    1559 Fem1+Tib1      1557    1559     1559          2
#> 1_12   m    1571 Fem1+Tib1      1568    1571     1571          2
#> 1_13   m    1583 Fem1+Tib1      1579    1583     1583          2
#> 1_14   m    1595 Fem1+Tib1      1591    1595     1595          2
#> 1_15   m    1608 Fem1+Tib1      1603    1608     1608          2
#> 1_16   m    1620 Fem1+Tib1      1615    1620     1620          2
#> 1_17   m    1632 Fem1+Tib1      1626    1632     1632          2
#> 1_18   m    1644 Fem1+Tib1      1638    1644     1644          2
#> 1_19   m    1656 Fem1+Tib1      1649    1656     1656          2
#> 1_2    m    1449 Fem1+Tib1      1452    1449     1448          2
#> 1_20   m    1669 Fem1+Tib1      1662    1669     1669          2
#> 1_21   m    1681 Fem1+Tib1      1673    1681     1681          2
#> 1_22   m    1693 Fem1+Tib1      1684    1693     1693          2
#> 1_23   m    1705 Fem1+Tib1      1696    1705     1705          2
#> 1_24   m    1717 Fem1+Tib1      1707    1717     1717          2
#> 1_25   m    1729 Fem1+Tib1      1718    1729     1729          2
#> 1_26   m    1741 Fem1+Tib1      1730    1741     1741          2
#> 1_27   m    1753 Fem1+Tib1      1741    1753     1752          2
#> 1_28   m    1765 Fem1+Tib1      1753    1765     1764          2
#> 1_29   m    1776 Fem1+Tib1      1764    1776     1776          2
#> 1_3    m    1461 Fem1+Tib1      1463    1461     1460          2
#> 1_30   m    1790 Fem1+Tib1      1777    1790     1790          2
#> 1_31   m    1802 Fem1+Tib1      1788    1802     1802          2
#> 1_32   m    1814 Fem1+Tib1      1799    1814     1814          2
#> 1_33   m    1826 Fem1+Tib1      1811    1826     1825          2
#> 1_34   m    1838 Fem1+Tib1      1822    1838     1837          2
#> 1_35   m    1851 Fem1+Tib1      1835    1851     1851          2
#> 1_36   m    1863 Fem1+Tib1      1846    1863     1863          2
#> 1_37   m    1875 Fem1+Tib1      1858    1875     1875          2
#> 1_38   m    1887 Fem1+Tib1      1869    1887     1887          2
#> 1_39   m    1899 Fem1+Tib1      1880    1899     1898          2
#> 1_4    m    1473 Fem1+Tib1      1474    1473     1472          2
#> 1_40   m    1912 Fem1+Tib1      1893    1912     1912          2
#> 1_41   m    1924 Fem1+Tib1      1904    1924     1924          2
#> 1_42   m    1936 Fem1+Tib1      1916    1936     1936          2
#> 1_43   m    1948 Fem1+Tib1      1927    1948     1948          2
#> 1_44   m    1960 Fem1+Tib1      1939    1960     1960          2
#> 1_45   m    1973 Fem1+Tib1      1951    1973     1973          2
#> 1_46   m    1985 Fem1+Tib1      1963    1985     1985          2
#> 1_47   m    1997 Fem1+Tib1      1974    1997     1997          2
#> 1_5    m    1486 Fem1+Tib1      1487    1486     1486          2
#> 1_6    m    1498 Fem1+Tib1      1498    1498     1498          2
#> 1_7    m    1510 Fem1+Tib1      1510    1510     1510          2
#> 1_8    m    1522 Fem1+Tib1      1521    1522     1522          2
#> 1_9    m    1534 Fem1+Tib1      1532    1534     1533          2
#> 3_1    f    1365 Fem1+Tib1      1365    1358     1358          2
#> 3_10   f    1460 Fem1+Tib1      1460    1458     1457          2
#> 3_11   f    1470 Fem1+Tib1      1470    1468     1468          2
#> 3_12   f    1481 Fem1+Tib1      1481    1480     1480          2
#> 3_13   f    1491 Fem1+Tib1      1491    1490     1490          2
#> 3_14   f    1501 Fem1+Tib1      1501    1501     1501          2
#> 3_15   f    1513 Fem1+Tib1      1513    1513     1513          2
#> 3_16   f    1524 Fem1+Tib1      1524    1525     1524          2
#> 3_17   f    1535 Fem1+Tib1      1535    1537     1536          2
#> 3_18   f    1545 Fem1+Tib1      1545    1547     1547          2
#> 3_19   f    1557 Fem1+Tib1      1557    1559     1559          2
#> 3_2    f    1375 Fem1+Tib1      1375    1368     1368          2
#> 3_20   f    1567 Fem1+Tib1      1567    1569     1569          2
#> 3_21   f    1578 Fem1+Tib1      1578    1581     1581          2
#> 3_22   f    1588 Fem1+Tib1      1588    1592     1592          2
#> 3_23   f    1598 Fem1+Tib1      1598    1602     1602          2
#> 3_24   f    1609 Fem1+Tib1      1609    1614     1614          2
#> 3_25   f    1619 Fem1+Tib1      1619    1625     1624          2
#> 3_26   f    1630 Fem1+Tib1      1630    1636     1636          2
#> 3_27   f    1640 Fem1+Tib1      1640    1647     1647          2
#> 3_28   f    1652 Fem1+Tib1      1652    1659     1659          2
#> 3_29   f    1662 Fem1+Tib1      1662    1669     1669          2
#> 3_3    f    1385 Fem1+Tib1      1385    1379     1378          2
#> 3_30   f    1673 Fem1+Tib1      1673    1681     1681          2
#> 3_31   f    1683 Fem1+Tib1      1683    1692     1691          2
#> 3_32   f    1693 Fem1+Tib1      1693    1702     1702          2
#> 3_33   f    1704 Fem1+Tib1      1704    1714     1714          2
#> 3_34   f    1714 Fem1+Tib1      1714    1724     1724          2
#> 3_35   f    1726 Fem1+Tib1      1726    1736     1736          2
#> 3_36   f    1736 Fem1+Tib1      1736    1747     1746          2
#> 3_37   f    1748 Fem1+Tib1      1748    1760     1760          2
#> 3_38   f    1758 Fem1+Tib1      1758    1771     1770          2
#> 3_39   f    1770 Fem1+Tib1      1770    1782     1782          2
#> 3_4    f    1396 Fem1+Tib1      1396    1391     1390          2
#> 3_40   f    1780 Fem1+Tib1      1780    1793     1793          2
#> 3_41   f    1791 Fem1+Tib1      1791    1805     1805          2
#> 3_42   f    1801 Fem1+Tib1      1801    1815     1815          2
#> 3_43   f    1811 Fem1+Tib1      1811    1826     1825          2
#> 3_44   f    1822 Fem1+Tib1      1822    1838     1837          2
#> 3_45   f    1832 Fem1+Tib1      1832    1848     1848          2
#> 3_5    f    1406 Fem1+Tib1      1406    1401     1401          2
#> 3_6    f    1417 Fem1+Tib1      1417    1413     1413          2
#> 3_7    f    1427 Fem1+Tib1      1427    1423     1423          2
#> 3_8    f    1439 Fem1+Tib1      1439    1435     1435          2
#> 3_9    f    1449 Fem1+Tib1      1449    1446     1446          2
#> 
# ruff_etal_2012_n(dl.tgb) # The alternative.
```
