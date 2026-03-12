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
#> 2_1    m    1520 4. Fem1      1480    1520     1500          1
#> 2_10   m    1611 4. Fem1      1578    1611     1594          1
#> 2_11   m    1619 4. Fem1      1587    1619     1603          1
#> 2_12   m    1630 4. Fem1      1599    1630     1614          1
#> 2_13   m    1640 4. Fem1      1610    1640     1625          1
#> 2_14   m    1651 4. Fem1      1621    1651     1636          1
#> 2_15   m    1659 4. Fem1      1630    1659     1645          1
#> 2_16   m    1670 4. Fem1      1642    1670     1656          1
#> 2_17   m    1680 4. Fem1      1653    1680     1667          1
#> 2_18   m    1691 4. Fem1      1665    1691     1678          1
#> 2_19   m    1699 4. Fem1      1674    1699     1687          1
#> 2_2    m    1529 4. Fem1      1489    1529     1509          1
#> 2_20   m    1710 4. Fem1      1685    1710     1698          1
#> 2_21   m    1721 4. Fem1      1697    1721     1709          1
#> 2_22   m    1731 4. Fem1      1708    1731     1720          1
#> 2_23   m    1740 4. Fem1      1717    1740     1728          1
#> 2_24   m    1750 4. Fem1      1728    1750     1739          1
#> 2_25   m    1761 4. Fem1      1740    1761     1750          1
#> 2_26   m    1771 4. Fem1      1751    1771     1761          1
#> 2_27   m    1780 4. Fem1      1760    1780     1770          1
#> 2_28   m    1790 4. Fem1      1772    1790     1781          1
#> 2_29   m    1801 4. Fem1      1783    1801     1792          1
#> 2_3    m    1539 4. Fem1      1500    1539     1520          1
#> 2_30   m    1811 4. Fem1      1795    1811     1803          1
#> 2_31   m    1820 4. Fem1      1804    1820     1812          1
#> 2_32   m    1830 4. Fem1      1815    1830     1823          1
#> 2_33   m    1841 4. Fem1      1827    1841     1834          1
#> 2_34   m    1851 4. Fem1      1838    1851     1845          1
#> 2_35   m    1860 4. Fem1      1847    1860     1853          1
#> 2_36   m    1870 4. Fem1      1858    1870     1864          1
#> 2_37   m    1881 4. Fem1      1870    1881     1875          1
#> 2_38   m    1891 4. Fem1      1881    1891     1886          1
#> 2_39   m    1900 4. Fem1      1890    1900     1895          1
#> 2_4    m    1550 4. Fem1      1512    1550     1531          1
#> 2_40   m    1910 4. Fem1      1902    1910     1906          1
#> 2_41   m    1921 4. Fem1      1913    1921     1917          1
#> 2_42   m    1932 4. Fem1      1925    1932     1928          1
#> 2_43   m    1940 4. Fem1      1934    1940     1937          1
#> 2_44   m    1951 4. Fem1      1945    1951     1948          1
#> 2_45   m    1961 4. Fem1      1956    1961     1959          1
#> 2_46   m    1972 4. Fem1      1968    1972     1970          1
#> 2_47   m    1980 4. Fem1      1977    1980     1979          1
#> 2_5    m    1560 4. Fem1      1523    1560     1542          1
#> 2_6    m    1569 4. Fem1      1532    1569     1550          1
#> 2_7    m    1579 4. Fem1      1544    1579     1561          1
#> 2_8    m    1590 4. Fem1      1555    1590     1572          1
#> 2_9    m    1600 4. Fem1      1567    1600     1583          1
#> 4_1    f    1400 4. Fem1      1400    1446     1423          1
#> 4_10   f    1489 4. Fem1      1489    1529     1509          1
#> 4_11   f    1500 4. Fem1      1500    1539     1520          1
#> 4_12   f    1510 4. Fem1      1510    1548     1529          1
#> 4_13   f    1521 4. Fem1      1521    1558     1540          1
#> 4_14   f    1530 4. Fem1      1530    1566     1548          1
#> 4_15   f    1539 4. Fem1      1539    1575     1557          1
#> 4_16   f    1551 4. Fem1      1551    1585     1568          1
#> 4_17   f    1560 4. Fem1      1560    1594     1577          1
#> 4_18   f    1569 4. Fem1      1569    1602     1586          1
#> 4_19   f    1580 4. Fem1      1580    1613     1597          1
#> 4_2    f    1409 4. Fem1      1409    1455     1432          1
#> 4_20   f    1589 4. Fem1      1589    1621     1605          1
#> 4_21   f    1601 4. Fem1      1601    1632     1616          1
#> 4_22   f    1610 4. Fem1      1610    1640     1625          1
#> 4_23   f    1619 4. Fem1      1619    1649     1634          1
#> 4_24   f    1630 4. Fem1      1630    1659     1645          1
#> 4_25   f    1640 4. Fem1      1640    1668     1654          1
#> 4_26   f    1651 4. Fem1      1651    1678     1665          1
#> 4_27   f    1660 4. Fem1      1660    1687     1673          1
#> 4_28   f    1669 4. Fem1      1669    1695     1682          1
#> 4_29   f    1681 4. Fem1      1681    1706     1693          1
#> 4_3    f    1421 4. Fem1      1421    1465     1443          1
#> 4_30   f    1690 4. Fem1      1690    1714     1702          1
#> 4_31   f    1701 4. Fem1      1701    1725     1713          1
#> 4_32   f    1710 4. Fem1      1710    1733     1722          1
#> 4_33   f    1719 4. Fem1      1719    1742     1730          1
#> 4_34   f    1731 4. Fem1      1731    1752     1741          1
#> 4_35   f    1740 4. Fem1      1740    1761     1750          1
#> 4_36   f    1749 4. Fem1      1749    1769     1759          1
#> 4_37   f    1760 4. Fem1      1760    1780     1770          1
#> 4_38   f    1770 4. Fem1      1770    1788     1779          1
#> 4_39   f    1781 4. Fem1      1781    1799     1790          1
#> 4_4    f    1430 4. Fem1      1430    1474     1452          1
#> 4_40   f    1790 4. Fem1      1790    1807     1799          1
#> 4_41   f    1799 4. Fem1      1799    1815     1807          1
#> 4_42   f    1811 4. Fem1      1811    1826     1818          1
#> 4_43   f    1820 4. Fem1      1820    1834     1827          1
#> 4_44   f    1831 4. Fem1      1831    1845     1838          1
#> 4_45   f    1840 4. Fem1      1840    1853     1847          1
#> 4_5    f    1439 4. Fem1      1439    1482     1461          1
#> 4_6    f    1450 4. Fem1      1450    1493     1471          1
#> 4_7    f    1459 4. Fem1      1459    1501     1480          1
#> 4_8    f    1471 4. Fem1      1471    1512     1491          1
#> 4_9    f    1480 4. Fem1      1480    1520     1500          1
#> 
# trotter_gleser_1952_an(dl.tgb) # The alternative.
```
