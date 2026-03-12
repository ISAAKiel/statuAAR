# Calculate stature estimation according to: Vercellotti et al. 2009

Stature estimation (mm) based on the hierarchy of different regression
calculations, separated by sex developed for 60 individuals from a
medieval (XI-XII c) cemetery in Giecz, Poland (Vercellotti et al. 2009).
Bone measures used: Fem2+Tib1, Fem2, Fem1, Tib1, Hum1+Rad1, Hum1, Rad1

If bone measures for left and right are provided the mean value will be
used, but for statistic information 2 bones will be counted
(n_measures). Vercellotti et al. propose regression calculations for the
combination of male and female individuals. These regressions will be
used in case of undetermined sex (indet.).

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
vercellotti_etal_2009(df)
```

## Arguments

- df:

  data.frame of type statuaar_data_table, containing informations on
  individual, bone and measurement.

## Value

data.frame with calculated stature and related information per
individual.

## References

Vercellotti G, Agnew AM, Justus HM, Sciulli PW (2009). “Stature
estimation in an early medieval (XI-XII c.) Polish population: Testing
the accuracy of regression equations in a bioarcheological sample.”
*American Journal of Physical Anthropology*, **140**(1), 135–142. ISSN
1096-8644, [doi:10.1002/ajpa.21055](https://doi.org/10.1002/ajpa.21055)
.

## Author

Hendrik Raese <h.raese@ufg.uni-kiel.de>

Christoph Rinne <crinne@ufg.uni-kiel.de>

Anna Loy <aloy@roots.uni-kiel.de>

Nils Müller-Scheeßel <nils.mueller-scheessel@ufg.uni-kiel.de>

## Examples

``` r
# Read example dataset into a data frame.
x <- statuAAR::TrotterGleser1952
x <- x[x$Race == "White", ]
#'
# Create & check the data frame of mesures concordance for Trotter & Gleser 1952
measures.concordance <- create.measures.concordance()
measures.concordance[measures.concordance$own != "",]
#>    short      long  own
#> 1   Fem1   Femur.1  Fem
#> 10  Hum1 Humerus.1  Hum
#> 19  Rad1  Radius.1  Rad
#> 28  Tib1   Tibia.1  Tib
#> 37  Uln1    Ulna.1 Ulna
#'
# Prepare statuaar_data_table
dl.tgb <- statuAAR::prep.statuaar.data(x, d.form = "wide", ind = "Appendix_row",
                                       sex = "Sex", measures.names = "own", stats = FALSE)
#'
# Calculate stature estimation using a given formula.
statuAAR::getStature(c("ve09"), dl.tgb)
#> $ve09
#>      sex stature bone if_female if_male if_indet n_measures
#> 1_1    m    1509 Fem1      1454    1509     1462          1
#> 1_10   m    1609 Fem1      1564    1609     1580          1
#> 1_11   m    1619 Fem1      1575    1619     1592          1
#> 1_12   m    1629 Fem1      1587    1629     1605          1
#> 1_13   m    1640 Fem1      1599    1640     1617          1
#> 1_14   m    1650 Fem1      1610    1650     1630          1
#> 1_15   m    1663 Fem1      1625    1663     1645          1
#> 1_16   m    1674 Fem1      1636    1674     1657          1
#> 1_17   m    1684 Fem1      1648    1684     1670          1
#> 1_18   m    1695 Fem1      1659    1695     1682          1
#> 1_19   m    1705 Fem1      1671    1705     1695          1
#> 1_2    m    1520 Fem1      1466    1520     1474          1
#> 1_20   m    1718 Fem1      1685    1718     1710          1
#> 1_21   m    1729 Fem1      1697    1729     1722          1
#> 1_22   m    1739 Fem1      1708    1739     1735          1
#> 1_23   m    1750 Fem1      1720    1750     1747          1
#> 1_24   m    1760 Fem1      1732    1760     1760          1
#> 1_25   m    1773 Fem1      1746    1773     1775          1
#> 1_26   m    1783 Fem1      1758    1783     1788          1
#> 1_27   m    1794 Fem1      1769    1794     1800          1
#> 1_28   m    1804 Fem1      1781    1804     1812          1
#> 1_29   m    1815 Fem1      1792    1815     1825          1
#> 1_3    m    1530 Fem1      1477    1530     1487          1
#> 1_30   m    1828 Fem1      1807    1828     1840          1
#> 1_31   m    1838 Fem1      1818    1838     1853          1
#> 1_32   m    1849 Fem1      1830    1849     1865          1
#> 1_33   m    1859 Fem1      1841    1859     1878          1
#> 1_34   m    1870 Fem1      1853    1870     1890          1
#> 1_35   m    1883 Fem1      1867    1883     1905          1
#> 1_36   m    1893 Fem1      1879    1893     1918          1
#> 1_37   m    1904 Fem1      1890    1904     1930          1
#> 1_38   m    1914 Fem1      1902    1914     1943          1
#> 1_39   m    1924 Fem1      1914    1924     1955          1
#> 1_4    m    1541 Fem1      1489    1541     1499          1
#> 1_40   m    1937 Fem1      1928    1937     1970          1
#> 1_41   m    1948 Fem1      1940    1948     1983          1
#> 1_42   m    1958 Fem1      1951    1958     1995          1
#> 1_43   m    1969 Fem1      1963    1969     2008          1
#> 1_44   m    1979 Fem1      1974    1979     2020          1
#> 1_45   m    1992 Fem1      1989    1992     2036          1
#> 1_46   m    2003 Fem1      2000    2003     2048          1
#> 1_47   m    2013 Fem1      2012    2013     2060          1
#> 1_5    m    1554 Fem1      1503    1554     1515          1
#> 1_6    m    1564 Fem1      1515    1564     1527          1
#> 1_7    m    1575 Fem1      1526    1575     1540          1
#> 1_8    m    1585 Fem1      1538    1585     1552          1
#> 1_9    m    1596 Fem1      1549    1596     1564          1
#> 3_1    f    1359 Fem1      1359    1423     1360          1
#> 3_10   f    1463 Fem1      1463    1517     1471          1
#> 3_11   f    1474 Fem1      1474    1528     1484          1
#> 3_12   f    1486 Fem1      1486    1538     1496          1
#> 3_13   f    1497 Fem1      1497    1549     1509          1
#> 3_14   f    1509 Fem1      1509    1559     1521          1
#> 3_15   f    1521 Fem1      1521    1569     1533          1
#> 3_16   f    1535 Fem1      1535    1582     1549          1
#> 3_17   f    1547 Fem1      1547    1593     1561          1
#> 3_18   f    1558 Fem1      1558    1603     1574          1
#> 3_19   f    1570 Fem1      1570    1614     1586          1
#> 3_2    f    1370 Fem1      1370    1434     1372          1
#> 3_20   f    1581 Fem1      1581    1624     1598          1
#> 3_21   f    1593 Fem1      1593    1635     1611          1
#> 3_22   f    1604 Fem1      1604    1645     1623          1
#> 3_23   f    1616 Fem1      1616    1656     1636          1
#> 3_24   f    1627 Fem1      1627    1666     1648          1
#> 3_25   f    1639 Fem1      1639    1676     1660          1
#> 3_26   f    1651 Fem1      1651    1687     1673          1
#> 3_27   f    1662 Fem1      1662    1697     1685          1
#> 3_28   f    1674 Fem1      1674    1708     1698          1
#> 3_29   f    1685 Fem1      1685    1718     1710          1
#> 3_3    f    1382 Fem1      1382    1444     1385          1
#> 3_30   f    1697 Fem1      1697    1729     1722          1
#> 3_31   f    1708 Fem1      1708    1739     1735          1
#> 3_32   f    1720 Fem1      1720    1750     1747          1
#> 3_33   f    1732 Fem1      1732    1760     1760          1
#> 3_34   f    1743 Fem1      1743    1770     1772          1
#> 3_35   f    1755 Fem1      1755    1781     1784          1
#> 3_36   f    1766 Fem1      1766    1791     1797          1
#> 3_37   f    1781 Fem1      1781    1804     1812          1
#> 3_38   f    1792 Fem1      1792    1815     1825          1
#> 3_39   f    1804 Fem1      1804    1825     1837          1
#> 3_4    f    1393 Fem1      1393    1455     1397          1
#> 3_40   f    1815 Fem1      1815    1836     1850          1
#> 3_41   f    1827 Fem1      1827    1846     1862          1
#> 3_42   f    1838 Fem1      1838    1857     1874          1
#> 3_43   f    1850 Fem1      1850    1867     1887          1
#> 3_44   f    1862 Fem1      1862    1877     1899          1
#> 3_45   f    1873 Fem1      1873    1888     1912          1
#> 3_5    f    1405 Fem1      1405    1465     1409          1
#> 3_6    f    1417 Fem1      1417    1475     1422          1
#> 3_7    f    1428 Fem1      1428    1486     1434          1
#> 3_8    f    1440 Fem1      1440    1496     1447          1
#> 3_9    f    1451 Fem1      1451    1507     1459          1
#> 
# vercellotti_etal_2009(dl.tgb) # The alternative.
```
