# Calculate stature estimation according to: Trotter & Gleser 'American White'.

Stature estimation (mm) based on the hierarchy of different regression
calculations, separated by sex (Trotter & Gleser 1952, 1977) series
'American White'. Bone measures used: Fem1, Tib1b, Fib1, Uln1, Rad1,
Hum1 If bone measures for left and right are provided the mean value
will be used, but for statistic information 2 bones will be counted
(n_measures). If sex is indet. the mean of male and female stature
estimation is given. The calculation is based on Trotter & Gleser (1952)
with corrections from Trotter & Gleser (1977). The regression formula
are hierarchical from combinations of different bone measures to single
bone measures. Only the first applicable measure will be used.

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
trotter_gleser_1952_aw(df)
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
statuAAR::getStature(c("tg02"), dl.tgb)
#> $tg02
#>      sex stature    bone if_female if_male if_indet n_measures
#> 1_1    m    1521 4. Fem1      1482    1521     1501          1
#> 1_10   m    1611 4. Fem1      1576    1611     1594          1
#> 1_11   m    1621 4. Fem1      1586    1621     1603          1
#> 1_12   m    1630 4. Fem1      1596    1630     1613          1
#> 1_13   m    1640 4. Fem1      1606    1640     1623          1
#> 1_14   m    1649 4. Fem1      1615    1649     1632          1
#> 1_15   m    1661 4. Fem1      1628    1661     1645          1
#> 1_16   m    1671 4. Fem1      1638    1671     1654          1
#> 1_17   m    1680 4. Fem1      1648    1680     1664          1
#> 1_18   m    1690 4. Fem1      1657    1690     1674          1
#> 1_19   m    1699 4. Fem1      1667    1699     1683          1
#> 1_2    m    1530 4. Fem1      1492    1530     1511          1
#> 1_20   m    1711 4. Fem1      1680    1711     1695          1
#> 1_21   m    1721 4. Fem1      1690    1721     1705          1
#> 1_22   m    1730 4. Fem1      1699    1730     1715          1
#> 1_23   m    1740 4. Fem1      1709    1740     1725          1
#> 1_24   m    1749 4. Fem1      1719    1749     1734          1
#> 1_25   m    1761 4. Fem1      1732    1761     1746          1
#> 1_26   m    1771 4. Fem1      1741    1771     1756          1
#> 1_27   m    1780 4. Fem1      1751    1780     1766          1
#> 1_28   m    1790 4. Fem1      1761    1790     1776          1
#> 1_29   m    1799 4. Fem1      1771    1799     1785          1
#> 1_3    m    1540 4. Fem1      1502    1540     1521          1
#> 1_30   m    1811 4. Fem1      1783    1811     1797          1
#> 1_31   m    1821 4. Fem1      1793    1821     1807          1
#> 1_32   m    1830 4. Fem1      1803    1830     1817          1
#> 1_33   m    1840 4. Fem1      1813    1840     1826          1
#> 1_34   m    1849 4. Fem1      1823    1849     1836          1
#> 1_35   m    1861 4. Fem1      1835    1861     1848          1
#> 1_36   m    1871 4. Fem1      1845    1871     1858          1
#> 1_37   m    1880 4. Fem1      1855    1880     1868          1
#> 1_38   m    1890 4. Fem1      1865    1890     1877          1
#> 1_39   m    1899 4. Fem1      1875    1899     1887          1
#> 1_4    m    1549 4. Fem1      1512    1549     1531          1
#> 1_40   m    1911 4. Fem1      1887    1911     1899          1
#> 1_41   m    1921 4. Fem1      1897    1921     1909          1
#> 1_42   m    1930 4. Fem1      1907    1930     1919          1
#> 1_43   m    1940 4. Fem1      1917    1940     1928          1
#> 1_44   m    1949 4. Fem1      1927    1949     1938          1
#> 1_45   m    1961 4. Fem1      1939    1961     1950          1
#> 1_46   m    1971 4. Fem1      1949    1971     1960          1
#> 1_47   m    1980 4. Fem1      1959    1980     1970          1
#> 1_5    m    1561 4. Fem1      1524    1561     1543          1
#> 1_6    m    1571 4. Fem1      1534    1571     1552          1
#> 1_7    m    1580 4. Fem1      1544    1580     1562          1
#> 1_8    m    1590 4. Fem1      1554    1590     1572          1
#> 1_9    m    1599 4. Fem1      1564    1599     1582          1
#> 3_1    f    1401 4. Fem1      1401    1442     1421          1
#> 3_10   f    1489 4. Fem1      1489    1528     1509          1
#> 3_11   f    1499 4. Fem1      1499    1538     1518          1
#> 3_12   f    1509 4. Fem1      1509    1547     1528          1
#> 3_13   f    1519 4. Fem1      1519    1557     1538          1
#> 3_14   f    1529 4. Fem1      1529    1566     1548          1
#> 3_15   f    1539 4. Fem1      1539    1576     1557          1
#> 3_16   f    1551 4. Fem1      1551    1588     1569          1
#> 3_17   f    1561 4. Fem1      1561    1597     1579          1
#> 3_18   f    1571 4. Fem1      1571    1607     1589          1
#> 3_19   f    1581 4. Fem1      1581    1616     1598          1
#> 3_2    f    1410 4. Fem1      1410    1452     1431          1
#> 3_20   f    1591 4. Fem1      1591    1626     1608          1
#> 3_21   f    1601 4. Fem1      1601    1635     1618          1
#> 3_22   f    1611 4. Fem1      1611    1645     1628          1
#> 3_23   f    1620 4. Fem1      1620    1654     1637          1
#> 3_24   f    1630 4. Fem1      1630    1664     1647          1
#> 3_25   f    1640 4. Fem1      1640    1673     1657          1
#> 3_26   f    1650 4. Fem1      1650    1683     1666          1
#> 3_27   f    1660 4. Fem1      1660    1692     1676          1
#> 3_28   f    1670 4. Fem1      1670    1702     1686          1
#> 3_29   f    1680 4. Fem1      1680    1711     1695          1
#> 3_3    f    1420 4. Fem1      1420    1461     1441          1
#> 3_30   f    1690 4. Fem1      1690    1721     1705          1
#> 3_31   f    1699 4. Fem1      1699    1730     1715          1
#> 3_32   f    1709 4. Fem1      1709    1740     1725          1
#> 3_33   f    1719 4. Fem1      1719    1749     1734          1
#> 3_34   f    1729 4. Fem1      1729    1759     1744          1
#> 3_35   f    1739 4. Fem1      1739    1768     1754          1
#> 3_36   f    1749 4. Fem1      1749    1778     1763          1
#> 3_37   f    1761 4. Fem1      1761    1790     1776          1
#> 3_38   f    1771 4. Fem1      1771    1799     1785          1
#> 3_39   f    1781 4. Fem1      1781    1809     1795          1
#> 3_4    f    1430 4. Fem1      1430    1471     1451          1
#> 3_40   f    1791 4. Fem1      1791    1818     1805          1
#> 3_41   f    1801 4. Fem1      1801    1828     1814          1
#> 3_42   f    1811 4. Fem1      1811    1837     1824          1
#> 3_43   f    1820 4. Fem1      1820    1847     1834          1
#> 3_44   f    1830 4. Fem1      1830    1856     1843          1
#> 3_45   f    1840 4. Fem1      1840    1866     1853          1
#> 3_5    f    1440 4. Fem1      1440    1480     1460          1
#> 3_6    f    1450 4. Fem1      1450    1490     1470          1
#> 3_7    f    1460 4. Fem1      1460    1499     1480          1
#> 3_8    f    1470 4. Fem1      1470    1509     1489          1
#> 3_9    f    1480 4. Fem1      1480    1518     1499          1
#> 
# trotter_gleser_1952_aw(dl.tgb) # The alternative.
```
