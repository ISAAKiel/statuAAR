# Calculate stature estimation according to: Ruff el al 2012, All (indet).

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

Bone measures (Fem1, Tib1, Hum1, Rad1) used in hierarchical order of
percent standard error of estimate (%SEE).

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
ruff_etal_2012_a(df)
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
statuAAR::getStature(c("r12a"), dl.tgb)
#> $r12a
#>      sex stature bone if_female if_male if_indet n_measures
#> 1_1    m    1465 Fem1      1460    1465     1460          1
#> 1_10   m    1568 Fem1      1563    1568     1566          1
#> 1_11   m    1579 Fem1      1573    1579     1577          1
#> 1_12   m    1590 Fem1      1584    1590     1588          1
#> 1_13   m    1601 Fem1      1595    1601     1599          1
#> 1_14   m    1612 Fem1      1606    1612     1610          1
#> 1_15   m    1625 Fem1      1619    1625     1624          1
#> 1_16   m    1636 Fem1      1630    1636     1635          1
#> 1_17   m    1647 Fem1      1641    1647     1646          1
#> 1_18   m    1658 Fem1      1651    1658     1657          1
#> 1_19   m    1669 Fem1      1662    1669     1668          1
#> 1_2    m    1476 Fem1      1471    1476     1471          1
#> 1_20   m    1682 Fem1      1676    1682     1682          1
#> 1_21   m    1693 Fem1      1686    1693     1693          1
#> 1_22   m    1704 Fem1      1697    1704     1704          1
#> 1_23   m    1715 Fem1      1708    1715     1715          1
#> 1_24   m    1726 Fem1      1719    1726     1726          1
#> 1_25   m    1740 Fem1      1732    1740     1740          1
#> 1_26   m    1750 Fem1      1743    1750     1751          1
#> 1_27   m    1761 Fem1      1754    1761     1762          1
#> 1_28   m    1772 Fem1      1764    1772     1773          1
#> 1_29   m    1783 Fem1      1775    1783     1784          1
#> 1_3    m    1487 Fem1      1482    1487     1483          1
#> 1_30   m    1797 Fem1      1789    1797     1798          1
#> 1_31   m    1808 Fem1      1799    1808     1809          1
#> 1_32   m    1818 Fem1      1810    1818     1820          1
#> 1_33   m    1829 Fem1      1821    1829     1832          1
#> 1_34   m    1840 Fem1      1832    1840     1843          1
#> 1_35   m    1854 Fem1      1845    1854     1856          1
#> 1_36   m    1865 Fem1      1856    1865     1868          1
#> 1_37   m    1876 Fem1      1867    1876     1879          1
#> 1_38   m    1886 Fem1      1877    1886     1890          1
#> 1_39   m    1897 Fem1      1888    1897     1901          1
#> 1_4    m    1497 Fem1      1493    1497     1494          1
#> 1_40   m    1911 Fem1      1902    1911     1915          1
#> 1_41   m    1922 Fem1      1912    1922     1926          1
#> 1_42   m    1933 Fem1      1923    1933     1937          1
#> 1_43   m    1944 Fem1      1934    1944     1948          1
#> 1_44   m    1954 Fem1      1945    1954     1959          1
#> 1_45   m    1968 Fem1      1958    1968     1973          1
#> 1_46   m    1979 Fem1      1969    1979     1984          1
#> 1_47   m    1990 Fem1      1980    1990     1995          1
#> 1_5    m    1511 Fem1      1506    1511     1507          1
#> 1_6    m    1522 Fem1      1517    1522     1519          1
#> 1_7    m    1533 Fem1      1528    1533     1530          1
#> 1_8    m    1544 Fem1      1538    1544     1541          1
#> 1_9    m    1555 Fem1      1549    1555     1552          1
#> 3_1    f    1372 Fem1      1372    1375     1369          1
#> 3_10   f    1469 Fem1      1469    1473     1469          1
#> 3_11   f    1479 Fem1      1479    1484     1480          1
#> 3_12   f    1490 Fem1      1490    1495     1491          1
#> 3_13   f    1501 Fem1      1501    1506     1502          1
#> 3_14   f    1512 Fem1      1512    1516     1513          1
#> 3_15   f    1522 Fem1      1522    1527     1524          1
#> 3_16   f    1536 Fem1      1536    1541     1538          1
#> 3_17   f    1547 Fem1      1547    1552     1549          1
#> 3_18   f    1557 Fem1      1557    1563     1560          1
#> 3_19   f    1568 Fem1      1568    1574     1571          1
#> 3_2    f    1382 Fem1      1382    1386     1380          1
#> 3_20   f    1579 Fem1      1579    1584     1582          1
#> 3_21   f    1590 Fem1      1590    1595     1593          1
#> 3_22   f    1600 Fem1      1600    1606     1604          1
#> 3_23   f    1611 Fem1      1611    1617     1615          1
#> 3_24   f    1622 Fem1      1622    1628     1627          1
#> 3_25   f    1633 Fem1      1633    1639     1638          1
#> 3_26   f    1643 Fem1      1643    1650     1649          1
#> 3_27   f    1654 Fem1      1654    1661     1660          1
#> 3_28   f    1665 Fem1      1665    1672     1671          1
#> 3_29   f    1676 Fem1      1676    1682     1682          1
#> 3_3    f    1393 Fem1      1393    1397     1391          1
#> 3_30   f    1686 Fem1      1686    1693     1693          1
#> 3_31   f    1697 Fem1      1697    1704     1704          1
#> 3_32   f    1708 Fem1      1708    1715     1715          1
#> 3_33   f    1719 Fem1      1719    1726     1726          1
#> 3_34   f    1729 Fem1      1729    1737     1737          1
#> 3_35   f    1740 Fem1      1740    1748     1748          1
#> 3_36   f    1751 Fem1      1751    1759     1760          1
#> 3_37   f    1764 Fem1      1764    1772     1773          1
#> 3_38   f    1775 Fem1      1775    1783     1784          1
#> 3_39   f    1786 Fem1      1786    1794     1796          1
#> 3_4    f    1404 Fem1      1404    1408     1402          1
#> 3_40   f    1797 Fem1      1797    1805     1807          1
#> 3_41   f    1808 Fem1      1808    1816     1818          1
#> 3_42   f    1818 Fem1      1818    1827     1829          1
#> 3_43   f    1829 Fem1      1829    1837     1840          1
#> 3_44   f    1840 Fem1      1840    1848     1851          1
#> 3_45   f    1851 Fem1      1851    1859     1862          1
#> 3_5    f    1415 Fem1      1415    1419     1413          1
#> 3_6    f    1426 Fem1      1426    1429     1424          1
#> 3_7    f    1436 Fem1      1436    1440     1435          1
#> 3_8    f    1447 Fem1      1447    1451     1447          1
#> 3_9    f    1458 Fem1      1458    1462     1458          1
#> 
# ruff_etal_2012_a(dl.tgb) # The alternative.
```
