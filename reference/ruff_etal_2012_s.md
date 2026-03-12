# Calculate stature estimation according to: Ruff el al 2012, South.

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
ruff_etal_2012_s(df)
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
statuAAR::getStature(c("r12s"), dl.tgb)
#> $r12s
#>      sex stature      bone if_female if_male if_indet n_measures
#> 1_1    m    1438 Fem1+Tib1      1417    1438     1425          2
#> 1_10   m    1541 Fem1+Tib1      1526    1541     1534          2
#> 1_11   m    1552 Fem1+Tib1      1538    1552     1546          2
#> 1_12   m    1564 Fem1+Tib1      1550    1564     1558          2
#> 1_13   m    1575 Fem1+Tib1      1562    1575     1570          2
#> 1_14   m    1586 Fem1+Tib1      1573    1586     1581          2
#> 1_15   m    1599 Fem1+Tib1      1586    1599     1595          2
#> 1_16   m    1610 Fem1+Tib1      1598    1610     1607          2
#> 1_17   m    1621 Fem1+Tib1      1610    1621     1618          2
#> 1_18   m    1632 Fem1+Tib1      1622    1632     1630          2
#> 1_19   m    1643 Fem1+Tib1      1634    1643     1642          2
#> 1_2    m    1449 Fem1+Tib1      1429    1449     1436          2
#> 1_20   m    1656 Fem1+Tib1      1647    1656     1655          2
#> 1_21   m    1667 Fem1+Tib1      1659    1667     1667          2
#> 1_22   m    1678 Fem1+Tib1      1670    1678     1679          2
#> 1_23   m    1690 Fem1+Tib1      1682    1690     1691          2
#> 1_24   m    1701 Fem1+Tib1      1694    1701     1703          2
#> 1_25   m    1712 Fem1+Tib1      1706    1712     1715          2
#> 1_26   m    1723 Fem1+Tib1      1717    1723     1726          2
#> 1_27   m    1734 Fem1+Tib1      1729    1734     1738          2
#> 1_28   m    1746 Fem1+Tib1      1741    1746     1750          2
#> 1_29   m    1757 Fem1+Tib1      1753    1757     1762          2
#> 1_3    m    1460 Fem1+Tib1      1441    1460     1448          2
#> 1_30   m    1769 Fem1+Tib1      1766    1769     1775          2
#> 1_31   m    1781 Fem1+Tib1      1778    1781     1787          2
#> 1_32   m    1792 Fem1+Tib1      1789    1792     1799          2
#> 1_33   m    1803 Fem1+Tib1      1801    1803     1811          2
#> 1_34   m    1814 Fem1+Tib1      1813    1814     1823          2
#> 1_35   m    1827 Fem1+Tib1      1826    1827     1836          2
#> 1_36   m    1838 Fem1+Tib1      1838    1838     1848          2
#> 1_37   m    1849 Fem1+Tib1      1850    1849     1860          2
#> 1_38   m    1860 Fem1+Tib1      1861    1860     1872          2
#> 1_39   m    1872 Fem1+Tib1      1873    1872     1883          2
#> 1_4    m    1471 Fem1+Tib1      1453    1471     1460          2
#> 1_40   m    1884 Fem1+Tib1      1886    1884     1897          2
#> 1_41   m    1895 Fem1+Tib1      1898    1895     1909          2
#> 1_42   m    1907 Fem1+Tib1      1910    1907     1920          2
#> 1_43   m    1918 Fem1+Tib1      1922    1918     1932          2
#> 1_44   m    1929 Fem1+Tib1      1933    1929     1944          2
#> 1_45   m    1942 Fem1+Tib1      1947    1942     1957          2
#> 1_46   m    1953 Fem1+Tib1      1958    1953     1969          2
#> 1_47   m    1964 Fem1+Tib1      1970    1964     1981          2
#> 1_5    m    1484 Fem1+Tib1      1466    1484     1473          2
#> 1_6    m    1495 Fem1+Tib1      1478    1495     1485          2
#> 1_7    m    1506 Fem1+Tib1      1489    1506     1497          2
#> 1_8    m    1517 Fem1+Tib1      1501    1517     1509          2
#> 1_9    m    1529 Fem1+Tib1      1513    1529     1521          2
#> 3_1    f    1340 Fem1+Tib1      1340    1363     1346          2
#> 3_10   f    1438 Fem1+Tib1      1438    1457     1445          2
#> 3_11   f    1448 Fem1+Tib1      1448    1467     1456          2
#> 3_12   f    1460 Fem1+Tib1      1460    1478     1467          2
#> 3_13   f    1470 Fem1+Tib1      1470    1488     1478          2
#> 3_14   f    1481 Fem1+Tib1      1481    1498     1488          2
#> 3_15   f    1492 Fem1+Tib1      1492    1509     1500          2
#> 3_16   f    1504 Fem1+Tib1      1504    1520     1512          2
#> 3_17   f    1516 Fem1+Tib1      1516    1531     1524          2
#> 3_18   f    1526 Fem1+Tib1      1526    1541     1534          2
#> 3_19   f    1538 Fem1+Tib1      1538    1552     1546          2
#> 3_2    f    1350 Fem1+Tib1      1350    1373     1356          2
#> 3_20   f    1548 Fem1+Tib1      1548    1562     1556          2
#> 3_21   f    1560 Fem1+Tib1      1560    1573     1568          2
#> 3_22   f    1570 Fem1+Tib1      1570    1583     1578          2
#> 3_23   f    1581 Fem1+Tib1      1581    1593     1589          2
#> 3_24   f    1592 Fem1+Tib1      1592    1604     1601          2
#> 3_25   f    1603 Fem1+Tib1      1603    1614     1611          2
#> 3_26   f    1614 Fem1+Tib1      1614    1625     1623          2
#> 3_27   f    1625 Fem1+Tib1      1625    1635     1633          2
#> 3_28   f    1636 Fem1+Tib1      1636    1646     1645          2
#> 3_29   f    1647 Fem1+Tib1      1647    1656     1655          2
#> 3_3    f    1360 Fem1+Tib1      1360    1383     1367          2
#> 3_30   f    1659 Fem1+Tib1      1659    1667     1667          2
#> 3_31   f    1669 Fem1+Tib1      1669    1677     1678          2
#> 3_32   f    1679 Fem1+Tib1      1679    1687     1688          2
#> 3_33   f    1691 Fem1+Tib1      1691    1698     1700          2
#> 3_34   f    1701 Fem1+Tib1      1701    1708     1710          2
#> 3_35   f    1713 Fem1+Tib1      1713    1719     1722          2
#> 3_36   f    1723 Fem1+Tib1      1723    1729     1732          2
#> 3_37   f    1736 Fem1+Tib1      1736    1741     1746          2
#> 3_38   f    1747 Fem1+Tib1      1747    1751     1756          2
#> 3_39   f    1758 Fem1+Tib1      1758    1762     1768          2
#> 3_4    f    1372 Fem1+Tib1      1372    1394     1379          2
#> 3_40   f    1769 Fem1+Tib1      1769    1772     1778          2
#> 3_41   f    1781 Fem1+Tib1      1781    1783     1790          2
#> 3_42   f    1791 Fem1+Tib1      1791    1793     1800          2
#> 3_43   f    1801 Fem1+Tib1      1801    1803     1811          2
#> 3_44   f    1813 Fem1+Tib1      1813    1814     1823          2
#> 3_45   f    1823 Fem1+Tib1      1823    1824     1833          2
#> 3_5    f    1382 Fem1+Tib1      1382    1404     1389          2
#> 3_6    f    1394 Fem1+Tib1      1394    1415     1401          2
#> 3_7    f    1404 Fem1+Tib1      1404    1425     1411          2
#> 3_8    f    1416 Fem1+Tib1      1416    1436     1423          2
#> 3_9    f    1426 Fem1+Tib1      1426    1446     1433          2
#> 
# ruff_etal_2012_s(dl.tgb) # The alternative.
```
