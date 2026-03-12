# Calculate stature estimation according to: Raxter el al 2008.

Stature estimation (mm) based on the hierarchy of different regression
calculations, separated by sex, based on Egyptian skeletal sample
consists of 63 adult males and 37 adult females (Raxter el al 2008).

Bone measures used in hierarchical order of correlation (r):

- male: Fem1+Tib1a, Fem2+Tib1a, Tib1a, Tib1b, Fem1, Fem2, Hum1+Rad1,
  Hum1, Rad1

- female: Tib1b, Tib1a, Fem2+Tib1b, Fem1+Tib1a, Fem2, Fem1, Hum1,
  Hum1+Rad1, Rad1

The addition of individual bone measurements is performed during the
calculation. If bone measures for left and right are provided the mean
value will be used, but for statistic information 2 bones will be
counted (n_measures). If sex is indet. the mean of male and female
stature estimation is given. The authors do not mention, if the mean of
all regressions is used or the first possible stature estimation
according to hierarchical order of correlation. As the authors discuss
the correlation and refere to the work of Trotter & Gleser a
hierarchical order is supposed (s. Siegmund 2010, p. 20). As correlation
within male and females is highly different and provides a sex specific
order of regressions the stature of individuals with indeterminated sex
(indet.) is calculated by the mean of both at each rank mentioning all
bone measures used and the sum of n_measures.

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

Christoph Rinne <crinne@ufg.uni-kiel.de>

Hendrik Raese <h.raese@ufg.uni-kiel.de>

## Examples

``` r
# Read example dataset into a data frame.
# Raxter et al. compare their data with Trotter & Gleser 1952 (Black)
x <- statuAAR::TrotterGleser1952
x <- x[x$Race == "Black", ]

# Create & check the data frame of mesures concordance for Trotter & Gleser 1952
measures.concordance <- create.measures.concordance()
measures.concordance[measures.concordance$own != "",]
#>    short      long  own
#> 1   Fem1   Femur.1  Fem
#> 10  Hum1 Humerus.1  Hum
#> 19  Rad1  Radius.1  Rad
#> 28  Tib1   Tibia.1  Tib
#> 37  Uln1    Ulna.1 Ulna

dl.tgb <- statuAAR::prep.statuaar.data(x, d.form = "wide", ind = "Appendix_row",
                                       sex = "Sex", measures.names = "own", stats = FALSE)

# Calculate stature estimation using a given formula.
# (Present: Tib1, required: Tib1a)
statuAAR::getStature(c("ra08"), dl.tgb)
#> $ra08
#>      sex stature bone if_female if_male if_indet n_measures
#> 2_1    m    1513 Fem1      1475    1513     1494          1
#> 2_10   m    1610 Fem1      1576    1610     1593          1
#> 2_11   m    1619 Fem1      1585    1619     1602          1
#> 2_12   m    1630 Fem1      1597    1630     1614          1
#> 2_13   m    1641 Fem1      1609    1641     1625          1
#> 2_14   m    1653 Fem1      1621    1653     1637          1
#> 2_15   m    1662 Fem1      1630    1662     1646          1
#> 2_16   m    1673 Fem1      1642    1673     1657          1
#> 2_17   m    1684 Fem1      1653    1684     1669          1
#> 2_18   m    1696 Fem1      1665    1696     1680          1
#> 2_19   m    1705 Fem1      1674    1705     1689          1
#> 2_2    m    1522 Fem1      1485    1522     1503          1
#> 2_20   m    1716 Fem1      1686    1716     1701          1
#> 2_21   m    1727 Fem1      1698    1727     1712          1
#> 2_22   m    1738 Fem1      1709    1738     1724          1
#> 2_23   m    1747 Fem1      1719    1747     1733          1
#> 2_24   m    1759 Fem1      1731    1759     1745          1
#> 2_25   m    1770 Fem1      1742    1770     1756          1
#> 2_26   m    1781 Fem1      1754    1781     1768          1
#> 2_27   m    1790 Fem1      1763    1790     1777          1
#> 2_28   m    1802 Fem1      1775    1802     1788          1
#> 2_29   m    1813 Fem1      1787    1813     1800          1
#> 2_3    m    1533 Fem1      1497    1533     1515          1
#> 2_30   m    1824 Fem1      1798    1824     1811          1
#> 2_31   m    1833 Fem1      1808    1833     1821          1
#> 2_32   m    1845 Fem1      1819    1845     1832          1
#> 2_33   m    1856 Fem1      1831    1856     1843          1
#> 2_34   m    1867 Fem1      1843    1867     1855          1
#> 2_35   m    1876 Fem1      1852    1876     1864          1
#> 2_36   m    1887 Fem1      1864    1887     1876          1
#> 2_37   m    1899 Fem1      1876    1899     1887          1
#> 2_38   m    1910 Fem1      1887    1910     1899          1
#> 2_39   m    1919 Fem1      1897    1919     1908          1
#> 2_4    m    1544 Fem1      1508    1544     1526          1
#> 2_40   m    1930 Fem1      1908    1930     1919          1
#> 2_41   m    1942 Fem1      1920    1942     1931          1
#> 2_42   m    1953 Fem1      1932    1953     1942          1
#> 2_43   m    1962 Fem1      1941    1962     1952          1
#> 2_44   m    1973 Fem1      1953    1973     1963          1
#> 2_45   m    1984 Fem1      1965    1984     1975          1
#> 2_46   m    1996 Fem1      1976    1996     1986          1
#> 2_47   m    2005 Fem1      1986    2005     1995          1
#> 2_5    m    1556 Fem1      1520    1556     1538          1
#> 2_6    m    1565 Fem1      1529    1565     1547          1
#> 2_7    m    1576 Fem1      1541    1576     1558          1
#> 2_8    m    1587 Fem1      1553    1587     1570          1
#> 2_9    m    1599 Fem1      1564    1599     1581          1
#> 4_1    f    1394 Fem1      1394    1434     1414          1
#> 4_10   f    1485 Fem1      1485    1522     1503          1
#> 4_11   f    1497 Fem1      1497    1533     1515          1
#> 4_12   f    1506 Fem1      1506    1542     1524          1
#> 4_13   f    1518 Fem1      1518    1553     1535          1
#> 4_14   f    1527 Fem1      1527    1562     1545          1
#> 4_15   f    1536 Fem1      1536    1571     1554          1
#> 4_16   f    1548 Fem1      1548    1583     1565          1
#> 4_17   f    1557 Fem1      1557    1592     1575          1
#> 4_18   f    1567 Fem1      1567    1601     1584          1
#> 4_19   f    1578 Fem1      1578    1612     1595          1
#> 4_2    f    1403 Fem1      1403    1443     1423          1
#> 4_20   f    1588 Fem1      1588    1621     1604          1
#> 4_21   f    1600 Fem1      1600    1632     1616          1
#> 4_22   f    1609 Fem1      1609    1641     1625          1
#> 4_23   f    1618 Fem1      1618    1650     1634          1
#> 4_24   f    1630 Fem1      1630    1662     1646          1
#> 4_25   f    1639 Fem1      1639    1671     1655          1
#> 4_26   f    1651 Fem1      1651    1682     1667          1
#> 4_27   f    1660 Fem1      1660    1691     1676          1
#> 4_28   f    1670 Fem1      1670    1700     1685          1
#> 4_29   f    1681 Fem1      1681    1711     1696          1
#> 4_3    f    1415 Fem1      1415    1454     1434          1
#> 4_30   f    1691 Fem1      1691    1720     1706          1
#> 4_31   f    1702 Fem1      1702    1732     1717          1
#> 4_32   f    1712 Fem1      1712    1741     1726          1
#> 4_33   f    1721 Fem1      1721    1750     1735          1
#> 4_34   f    1733 Fem1      1733    1761     1747          1
#> 4_35   f    1742 Fem1      1742    1770     1756          1
#> 4_36   f    1752 Fem1      1752    1779     1765          1
#> 4_37   f    1763 Fem1      1763    1790     1777          1
#> 4_38   f    1773 Fem1      1773    1799     1786          1
#> 4_39   f    1784 Fem1      1784    1811     1798          1
#> 4_4    f    1424 Fem1      1424    1463     1444          1
#> 4_40   f    1794 Fem1      1794    1820     1807          1
#> 4_41   f    1803 Fem1      1803    1829     1816          1
#> 4_42   f    1815 Fem1      1815    1840     1827          1
#> 4_43   f    1824 Fem1      1824    1849     1837          1
#> 4_44   f    1836 Fem1      1836    1860     1848          1
#> 4_45   f    1845 Fem1      1845    1869     1857          1
#> 4_5    f    1433 Fem1      1433    1472     1453          1
#> 4_6    f    1445 Fem1      1445    1483     1464          1
#> 4_7    f    1454 Fem1      1454    1492     1473          1
#> 4_8    f    1466 Fem1      1466    1504     1485          1
#> 4_9    f    1475 Fem1      1475    1513     1494          1
#> 
# raxter_etal_2008(dl.tgb) # The alternative.
```
