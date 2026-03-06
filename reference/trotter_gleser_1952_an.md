# Calculate stature estimation according to: Trotter & Gleser 'American Negro'.

Stature estimation (mm) based on the hierarchy of different regression
calculations, separated by sex (Trotter & Gleser 1952, 1977) series
'American Negro'. Bone measures used: Fem1, Tib1b, Fib1, Uln1, Rad1,
Hum1 If bone measures for left and right are provided the mean value
will be used, but for statistic information 2 bones will be counted
(n_measures). If sex is indet. the mean of male and female stature
estimation is given. The calculation is based on Trotter & Gleser (1952)
with corrections from Trotter & Gleser (1977). The regression formula
are hierarchical from combinations of different bone measures to single
bone measures. Only the first applicable measure will be used (Siegmund
2010, 112).

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
trotter_gleser_1952_an(df)
```

## Arguments

- df:

  data.frame of type statuaar_data_table, containing informations on
  individual, bone and measurement.

## Value

data.frame with calculated stature and related information per
individual.

## References

Trotter M, Gleser GC (1952). “Estimation of stature from long bones of
American Whites and Negroes.” *American Journal of Physical
Anthropology*, **10**(4), 463–514. ISSN 1096-8644,
[doi:10.1002/ajpa.1330100407](https://doi.org/10.1002/ajpa.1330100407) .

Trotter M, Gleser GC (1977). “Corrigenda to “estimation of stature from
long limb bones of American Whites and Negroes,” American Journal
Physical Anthropology (1952).” *American Journal of Physical
Anthropology*, **47**(2), 355–356. ISSN 1096-8644,
[doi:10.1002/ajpa.1330470216](https://doi.org/10.1002/ajpa.1330470216) .

Siegmund F (2010). *Die Körpergröße der Menschen in der Ur- und
Frühgeschichte Mitteleuropas und ein Vergleich ihrer anthropologischen
Schätzmethoden*. Books on Demand, Norderstedt. ISBN 978-3-8391-5314-7,
tex.ids: siegmundKorpergrosseMenschenUr2010a.

## Author

Hendrik Raese <h.raese@ufg.uni-kiel.de>

Christoph Rinne <crinne@ufg.uni-kiel.de>

## Examples

``` r
# Read example dataset into a data frame.
x <- statuAAR::TrotterGleser1952
x <- x[x$Race == "Black", ]
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
statuAAR::getStature(c("tg01"), dl.tgb)
#> $tg01
#>      sex stature    bone if_female if_male if_indet n_measures
#> 2_1    m    1535 4. Fem1      1497    1535     1516          1
#> 2_10   m    1638 4. Fem1      1603    1638     1620          1
#> 2_11   m    1647 4. Fem1      1613    1647     1630          1
#> 2_12   m    1659 4. Fem1      1625    1659     1642          1
#> 2_13   m    1671 4. Fem1      1638    1671     1654          1
#> 2_14   m    1683 4. Fem1      1650    1683     1666          1
#> 2_15   m    1692 4. Fem1      1660    1692     1676          1
#> 2_16   m    1704 4. Fem1      1672    1704     1688          1
#> 2_17   m    1716 4. Fem1      1685    1716     1700          1
#> 2_18   m    1728 4. Fem1      1697    1728     1712          1
#> 2_19   m    1737 4. Fem1      1707    1737     1722          1
#> 2_2    m    1545 4. Fem1      1507    1545     1526          1
#> 2_20   m    1749 4. Fem1      1719    1749     1734          1
#> 2_21   m    1761 4. Fem1      1732    1761     1746          1
#> 2_22   m    1773 4. Fem1      1744    1773     1759          1
#> 2_23   m    1783 4. Fem1      1754    1783     1768          1
#> 2_24   m    1795 4. Fem1      1766    1795     1780          1
#> 2_25   m    1806 4. Fem1      1778    1806     1792          1
#> 2_26   m    1818 4. Fem1      1791    1818     1805          1
#> 2_27   m    1828 4. Fem1      1801    1828     1814          1
#> 2_28   m    1840 4. Fem1      1813    1840     1826          1
#> 2_29   m    1852 4. Fem1      1825    1852     1839          1
#> 2_3    m    1557 4. Fem1      1519    1557     1538          1
#> 2_30   m    1864 4. Fem1      1838    1864     1851          1
#> 2_31   m    1873 4. Fem1      1848    1873     1860          1
#> 2_32   m    1885 4. Fem1      1860    1885     1872          1
#> 2_33   m    1897 4. Fem1      1872    1897     1885          1
#> 2_34   m    1909 4. Fem1      1885    1909     1897          1
#> 2_35   m    1918 4. Fem1      1895    1918     1906          1
#> 2_36   m    1930 4. Fem1      1907    1930     1919          1
#> 2_37   m    1942 4. Fem1      1919    1942     1931          1
#> 2_38   m    1954 4. Fem1      1932    1954     1943          1
#> 2_39   m    1964 4. Fem1      1941    1964     1953          1
#> 2_4    m    1568 4. Fem1      1531    1568     1550          1
#> 2_40   m    1975 4. Fem1      1954    1975     1965          1
#> 2_41   m    1987 4. Fem1      1966    1987     1977          1
#> 2_42   m    1999 4. Fem1      1979    1999     1989          1
#> 2_43   m    2009 4. Fem1      1988    2009     1999          1
#> 2_44   m    2021 4. Fem1      2001    2021     2011          1
#> 2_45   m    2033 4. Fem1      2013    2033     2023          1
#> 2_46   m    2044 4. Fem1      2025    2044     2035          1
#> 2_47   m    2054 4. Fem1      2035    2054     2045          1
#> 2_5    m    1580 4. Fem1      1544    1580     1562          1
#> 2_6    m    1590 4. Fem1      1554    1590     1572          1
#> 2_7    m    1602 4. Fem1      1566    1602     1584          1
#> 2_8    m    1614 4. Fem1      1578    1614     1596          1
#> 2_9    m    1626 4. Fem1      1591    1626     1608          1
#> 4_1    f    1410 4. Fem1      1410    1452     1431          1
#> 4_10   f    1507 4. Fem1      1507    1545     1526          1
#> 4_11   f    1519 4. Fem1      1519    1557     1538          1
#> 4_12   f    1529 4. Fem1      1529    1566     1548          1
#> 4_13   f    1541 4. Fem1      1541    1578     1560          1
#> 4_14   f    1551 4. Fem1      1551    1588     1569          1
#> 4_15   f    1561 4. Fem1      1561    1597     1579          1
#> 4_16   f    1573 4. Fem1      1573    1609     1591          1
#> 4_17   f    1583 4. Fem1      1583    1618     1601          1
#> 4_18   f    1593 4. Fem1      1593    1628     1611          1
#> 4_19   f    1606 4. Fem1      1606    1640     1623          1
#> 4_2    f    1420 4. Fem1      1420    1461     1441          1
#> 4_20   f    1615 4. Fem1      1615    1649     1632          1
#> 4_21   f    1628 4. Fem1      1628    1661     1645          1
#> 4_22   f    1638 4. Fem1      1638    1671     1654          1
#> 4_23   f    1648 4. Fem1      1648    1680     1664          1
#> 4_24   f    1660 4. Fem1      1660    1692     1676          1
#> 4_25   f    1670 4. Fem1      1670    1702     1686          1
#> 4_26   f    1682 4. Fem1      1682    1714     1698          1
#> 4_27   f    1692 4. Fem1      1692    1723     1708          1
#> 4_28   f    1702 4. Fem1      1702    1733     1717          1
#> 4_29   f    1714 4. Fem1      1714    1745     1729          1
#> 4_3    f    1433 4. Fem1      1433    1473     1453          1
#> 4_30   f    1724 4. Fem1      1724    1754     1739          1
#> 4_31   f    1736 4. Fem1      1736    1766     1751          1
#> 4_32   f    1746 4. Fem1      1746    1776     1761          1
#> 4_33   f    1756 4. Fem1      1756    1785     1771          1
#> 4_34   f    1769 4. Fem1      1769    1797     1783          1
#> 4_35   f    1778 4. Fem1      1778    1806     1792          1
#> 4_36   f    1788 4. Fem1      1788    1816     1802          1
#> 4_37   f    1801 4. Fem1      1801    1828     1814          1
#> 4_38   f    1811 4. Fem1      1811    1837     1824          1
#> 4_39   f    1823 4. Fem1      1823    1849     1836          1
#> 4_4    f    1443 4. Fem1      1443    1483     1463          1
#> 4_40   f    1833 4. Fem1      1833    1859     1846          1
#> 4_41   f    1843 4. Fem1      1843    1868     1856          1
#> 4_42   f    1855 4. Fem1      1855    1880     1868          1
#> 4_43   f    1865 4. Fem1      1865    1890     1877          1
#> 4_44   f    1877 4. Fem1      1877    1902     1889          1
#> 4_45   f    1887 4. Fem1      1887    1911     1899          1
#> 4_5    f    1452 4. Fem1      1452    1492     1472          1
#> 4_6    f    1465 4. Fem1      1465    1504     1484          1
#> 4_7    f    1475 4. Fem1      1475    1514     1494          1
#> 4_8    f    1487 4. Fem1      1487    1526     1506          1
#> 4_9    f    1497 4. Fem1      1497    1535     1516          1
#> 
# trotter_gleser_1952_an(dl.tgb) # The alternative.
```
