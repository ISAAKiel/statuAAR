# Calculate stature estimation according to: Boldsen 1984

Stature estimation (mm) based on the mean of regression calculations,
separated by sex (Boldsen 1984). Bone measures used: Fem1, Tib1

If bone measures for left and right are provided the mean value will be
used, but for statistic information 2 bones will be counted
(n_measures). If sex is indet. the mean of male and female stature
estimation is given.

From a skeleton series of approximately 500 individuals from the
medieval burial ground of Lille Sct. Mikkelsgade in Viborg, the stature
of 65 very well-preserved adult skeletons (31 male, 34 female) were
determined (Boldsen 1984, Appendix 1). Based on these stature estimates,
four regression functions are presented: separated by gender for Fem1
and Tib1. Due to almost identical slopes, these functions are aggregated
according to bone type and separated only by the intercept for modern
populations (Danes, Finns) and the White Americans data from Trotter &
Gleser (1958). Here, we use the formulas obtained from the primary
archaeological data, the results of which are averaged when both bone
measurements are available.

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
boldsen_1984(df)
```

## Arguments

- df:

  data.frame of type statuaar_data_table, containing informations on
  individual, bone and measurement.

## Value

data.frame with calculated stature and related information per
individual.

## References

Boldsen J (1984). “A statistical evaluation of the basis for predicting
stature from lengths of long bones in European populations.” *American
Journal of Physical Anthropology*, **65**(3), 305–311. ISSN 1096-8644,
[doi:10.1002/ajpa.1330650310](https://doi.org/10.1002/ajpa.1330650310) ,
<https://onlinelibrary.wiley.com/doi/abs/10.1002/ajpa.1330650310>.

## Author

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
dl.tgw <- statuAAR::prep.statuaar.data(x, d.form = "wide", ind = "Appendix_row",
                             sex = "Sex", measures.names = "own", stats = FALSE)
#'
# Calculate stature estimation using a given formula.
statuAAR::getStature(c("bo84"), dl.tgw)
#> $bo84
#>      sex stature       bone if_female if_male if_indet n_measures
#> 1_1    m    1506 Fem1, Tib1      1457    1506     1482          2
#> 1_10   m    1597 Fem1, Tib1      1557    1597     1577          2
#> 1_11   m    1607 Fem1, Tib1      1568    1607     1587          2
#> 1_12   m    1617 Fem1, Tib1      1578    1617     1598          2
#> 1_13   m    1627 Fem1, Tib1      1589    1627     1608          2
#> 1_14   m    1637 Fem1, Tib1      1600    1637     1618          2
#> 1_15   m    1648 Fem1, Tib1      1612    1648     1630          2
#> 1_16   m    1658 Fem1, Tib1      1623    1658     1640          2
#> 1_17   m    1667 Fem1, Tib1      1634    1667     1650          2
#> 1_18   m    1677 Fem1, Tib1      1644    1677     1661          2
#> 1_19   m    1687 Fem1, Tib1      1655    1687     1671          2
#> 1_2    m    1516 Fem1, Tib1      1468    1516     1492          2
#> 1_20   m    1698 Fem1, Tib1      1667    1698     1683          2
#> 1_21   m    1708 Fem1, Tib1      1678    1708     1693          2
#> 1_22   m    1718 Fem1, Tib1      1689    1718     1703          2
#> 1_23   m    1728 Fem1, Tib1      1700    1728     1714          2
#> 1_24   m    1738 Fem1, Tib1      1710    1738     1724          2
#> 1_25   m    1748 Fem1, Tib1      1721    1748     1734          2
#> 1_26   m    1757 Fem1, Tib1      1732    1757     1745          2
#> 1_27   m    1767 Fem1, Tib1      1743    1767     1755          2
#> 1_28   m    1777 Fem1, Tib1      1753    1777     1765          2
#> 1_29   m    1787 Fem1, Tib1      1764    1787     1776          2
#> 1_3    m    1526 Fem1, Tib1      1479    1526     1502          2
#> 1_30   m    1798 Fem1, Tib1      1776    1798     1787          2
#> 1_31   m    1808 Fem1, Tib1      1787    1808     1797          2
#> 1_32   m    1818 Fem1, Tib1      1798    1818     1808          2
#> 1_33   m    1828 Fem1, Tib1      1809    1828     1818          2
#> 1_34   m    1837 Fem1, Tib1      1819    1837     1828          2
#> 1_35   m    1849 Fem1, Tib1      1831    1849     1840          2
#> 1_36   m    1858 Fem1, Tib1      1842    1858     1850          2
#> 1_37   m    1868 Fem1, Tib1      1853    1868     1861          2
#> 1_38   m    1878 Fem1, Tib1      1864    1878     1871          2
#> 1_39   m    1888 Fem1, Tib1      1875    1888     1881          2
#> 1_4    m    1536 Fem1, Tib1      1489    1536     1513          2
#> 1_40   m    1899 Fem1, Tib1      1887    1899     1893          2
#> 1_41   m    1909 Fem1, Tib1      1898    1909     1903          2
#> 1_42   m    1919 Fem1, Tib1      1908    1919     1914          2
#> 1_43   m    1929 Fem1, Tib1      1919    1929     1924          2
#> 1_44   m    1938 Fem1, Tib1      1930    1938     1934          2
#> 1_45   m    1950 Fem1, Tib1      1942    1950     1946          2
#> 1_46   m    1959 Fem1, Tib1      1953    1959     1956          2
#> 1_47   m    1969 Fem1, Tib1      1964    1969     1966          2
#> 1_5    m    1547 Fem1, Tib1      1502    1547     1524          2
#> 1_6    m    1557 Fem1, Tib1      1512    1557     1534          2
#> 1_7    m    1566 Fem1, Tib1      1523    1566     1545          2
#> 1_8    m    1576 Fem1, Tib1      1534    1576     1555          2
#> 1_9    m    1586 Fem1, Tib1      1545    1586     1565          2
#> 3_1    f    1387 Fem1, Tib1      1387    1440     1414          2
#> 3_10   f    1477 Fem1, Tib1      1477    1523     1500          2
#> 3_11   f    1486 Fem1, Tib1      1486    1532     1509          2
#> 3_12   f    1497 Fem1, Tib1      1497    1542     1519          2
#> 3_13   f    1506 Fem1, Tib1      1506    1550     1528          2
#> 3_14   f    1516 Fem1, Tib1      1516    1559     1537          2
#> 3_15   f    1526 Fem1, Tib1      1526    1569     1548          2
#> 3_16   f    1537 Fem1, Tib1      1537    1579     1558          2
#> 3_17   f    1548 Fem1, Tib1      1548    1588     1568          2
#> 3_18   f    1557 Fem1, Tib1      1557    1597     1577          2
#> 3_19   f    1568 Fem1, Tib1      1568    1607     1587          2
#> 3_2    f    1396 Fem1, Tib1      1396    1449     1423          2
#> 3_20   f    1577 Fem1, Tib1      1577    1616     1596          2
#> 3_21   f    1588 Fem1, Tib1      1588    1625     1607          2
#> 3_22   f    1597 Fem1, Tib1      1597    1634     1616          2
#> 3_23   f    1607 Fem1, Tib1      1607    1643     1625          2
#> 3_24   f    1618 Fem1, Tib1      1618    1653     1635          2
#> 3_25   f    1627 Fem1, Tib1      1627    1661     1644          2
#> 3_26   f    1638 Fem1, Tib1      1638    1671     1654          2
#> 3_27   f    1647 Fem1, Tib1      1647    1680     1663          2
#> 3_28   f    1658 Fem1, Tib1      1658    1690     1674          2
#> 3_29   f    1667 Fem1, Tib1      1667    1698     1683          2
#> 3_3    f    1405 Fem1, Tib1      1405    1458     1432          2
#> 3_30   f    1678 Fem1, Tib1      1678    1708     1693          2
#> 3_31   f    1687 Fem1, Tib1      1687    1717     1702          2
#> 3_32   f    1697 Fem1, Tib1      1697    1725     1711          2
#> 3_33   f    1708 Fem1, Tib1      1708    1735     1721          2
#> 3_34   f    1717 Fem1, Tib1      1717    1744     1730          2
#> 3_35   f    1728 Fem1, Tib1      1728    1754     1741          2
#> 3_36   f    1737 Fem1, Tib1      1737    1762     1750          2
#> 3_37   f    1749 Fem1, Tib1      1749    1773     1761          2
#> 3_38   f    1758 Fem1, Tib1      1758    1782     1770          2
#> 3_39   f    1769 Fem1, Tib1      1769    1792     1781          2
#> 3_4    f    1416 Fem1, Tib1      1416    1468     1442          2
#> 3_40   f    1779 Fem1, Tib1      1779    1801     1790          2
#> 3_41   f    1789 Fem1, Tib1      1789    1810     1800          2
#> 3_42   f    1799 Fem1, Tib1      1799    1819     1809          2
#> 3_43   f    1808 Fem1, Tib1      1808    1828     1818          2
#> 3_44   f    1819 Fem1, Tib1      1819    1838     1828          2
#> 3_45   f    1828 Fem1, Tib1      1828    1846     1837          2
#> 3_5    f    1426 Fem1, Tib1      1426    1476     1451          2
#> 3_6    f    1436 Fem1, Tib1      1436    1486     1461          2
#> 3_7    f    1446 Fem1, Tib1      1446    1495     1470          2
#> 3_8    f    1456 Fem1, Tib1      1456    1505     1481          2
#> 3_9    f    1466 Fem1, Tib1      1466    1513     1490          2
#> 
# boldsen_1984(dl.tgw) # The alternative.
```
