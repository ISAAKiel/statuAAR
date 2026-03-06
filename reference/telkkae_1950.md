# Calculate stature estimation according to: Telkkã 1950.

Stature estimation (mm) based on the mean of different regression
calculations, separated by sex. Bone measures used: Hum1, Rad2, Uln2,
Fem1, Tib1, Fib1

If bone measures for left and right are provided the mean value will be
used, but for statistic information 2 bones will be counted
(n_measures). If sex is indet. the mean of male and female stature
estimation is given. To retrieve the estimated stature 20 mm will be
substracted from the resulting mean value.

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
telkkae_1950(df)
```

## Arguments

- df:

  data.frame of type statuaar_data_table, containing informations on
  individual, bone and measurement.

## Value

data.frame with calculated stature and related information per
individual.

## References

Telkkã A (1950). “On the prediction of human stature from the long
bones.” *Acta Anatomica*, **9**, 103–117.

## Author

Christoph Rinne <crinne@ufg.uni-kiel.de>

## Examples

``` r
# Read example dataset into a data frame.
x <- statuAAR::TrotterGleser1952

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
statuAAR::getStature(c("te50"), dl.tgb)
#> $te50
#>      sex stature             bone if_female if_male if_indet n_measures
#> 1_1    m    1513 Hum1, Fem1, Tib1      1463    1513     1488          3
#> 1_10   m    1589 Hum1, Fem1, Tib1      1532    1589     1560          3
#> 1_11   m    1600 Hum1, Fem1, Tib1      1542    1600     1571          3
#> 1_12   m    1609 Hum1, Fem1, Tib1      1551    1609     1580          3
#> 1_13   m    1618 Hum1, Fem1, Tib1      1558    1618     1588          3
#> 1_14   m    1626 Hum1, Fem1, Tib1      1566    1626     1596          3
#> 1_15   m    1635 Hum1, Fem1, Tib1      1574    1635     1605          3
#> 1_16   m    1645 Hum1, Fem1, Tib1      1583    1645     1614          3
#> 1_17   m    1653 Hum1, Fem1, Tib1      1590    1653     1622          3
#> 1_18   m    1661 Hum1, Fem1, Tib1      1598    1661     1630          3
#> 1_19   m    1670 Hum1, Fem1, Tib1      1605    1670     1638          3
#> 1_2    m    1521 Hum1, Fem1, Tib1      1470    1521     1496          3
#> 1_20   m    1680 Hum1, Fem1, Tib1      1615    1680     1647          3
#> 1_21   m    1688 Hum1, Fem1, Tib1      1622    1688     1655          3
#> 1_22   m    1697 Hum1, Fem1, Tib1      1630    1697     1663          3
#> 1_23   m    1705 Hum1, Fem1, Tib1      1638    1705     1671          3
#> 1_24   m    1713 Hum1, Fem1, Tib1      1645    1713     1679          3
#> 1_25   m    1723 Hum1, Fem1, Tib1      1654    1723     1688          3
#> 1_26   m    1731 Hum1, Fem1, Tib1      1661    1731     1696          3
#> 1_27   m    1740 Hum1, Fem1, Tib1      1669    1740     1704          3
#> 1_28   m    1748 Hum1, Fem1, Tib1      1677    1748     1712          3
#> 1_29   m    1757 Hum1, Fem1, Tib1      1685    1757     1721          3
#> 1_3    m    1530 Hum1, Fem1, Tib1      1478    1530     1504          3
#> 1_30   m    1766 Hum1, Fem1, Tib1      1693    1766     1730          3
#> 1_31   m    1775 Hum1, Fem1, Tib1      1701    1775     1738          3
#> 1_32   m    1783 Hum1, Fem1, Tib1      1709    1783     1746          3
#> 1_33   m    1793 Hum1, Fem1, Tib1      1717    1793     1755          3
#> 1_34   m    1801 Hum1, Fem1, Tib1      1725    1801     1763          3
#> 1_35   m    1810 Hum1, Fem1, Tib1      1733    1810     1772          3
#> 1_36   m    1818 Hum1, Fem1, Tib1      1741    1818     1780          3
#> 1_37   m    1828 Hum1, Fem1, Tib1      1749    1828     1788          3
#> 1_38   m    1836 Hum1, Fem1, Tib1      1757    1836     1796          3
#> 1_39   m    1845 Hum1, Fem1, Tib1      1764    1845     1804          3
#> 1_4    m    1539 Hum1, Fem1, Tib1      1486    1539     1513          3
#> 1_40   m    1854 Hum1, Fem1, Tib1      1773    1854     1813          3
#> 1_41   m    1863 Hum1, Fem1, Tib1      1781    1863     1822          3
#> 1_42   m    1871 Hum1, Fem1, Tib1      1789    1871     1830          3
#> 1_43   m    1880 Hum1, Fem1, Tib1      1796    1880     1838          3
#> 1_44   m    1888 Hum1, Fem1, Tib1      1804    1888     1846          3
#> 1_45   m    1898 Hum1, Fem1, Tib1      1813    1898     1856          3
#> 1_46   m    1907 Hum1, Fem1, Tib1      1821    1907     1864          3
#> 1_47   m    1915 Hum1, Fem1, Tib1      1828    1915     1872          3
#> 1_5    m    1548 Hum1, Fem1, Tib1      1495    1548     1521          3
#> 1_6    m    1556 Hum1, Fem1, Tib1      1502    1556     1529          3
#> 1_7    m    1565 Hum1, Fem1, Tib1      1510    1565     1537          3
#> 1_8    m    1574 Hum1, Fem1, Tib1      1518    1574     1546          3
#> 1_9    m    1583 Hum1, Fem1, Tib1      1526    1583     1554          3
#> 2_1    m    1534 Hum1, Fem1, Tib1      1482    1534     1508          3
#> 2_10   m    1618 Hum1, Fem1, Tib1      1559    1618     1588          3
#> 2_11   m    1627 Hum1, Fem1, Tib1      1567    1627     1597          3
#> 2_12   m    1638 Hum1, Fem1, Tib1      1577    1638     1607          3
#> 2_13   m    1647 Hum1, Fem1, Tib1      1585    1647     1616          3
#> 2_14   m    1657 Hum1, Fem1, Tib1      1594    1657     1625          3
#> 2_15   m    1665 Hum1, Fem1, Tib1      1601    1665     1633          3
#> 2_16   m    1675 Hum1, Fem1, Tib1      1610    1675     1643          3
#> 2_17   m    1684 Hum1, Fem1, Tib1      1618    1684     1651          3
#> 2_18   m    1694 Hum1, Fem1, Tib1      1627    1694     1661          3
#> 2_19   m    1702 Hum1, Fem1, Tib1      1635    1702     1669          3
#> 2_2    m    1543 Hum1, Fem1, Tib1      1491    1543     1517          3
#> 2_20   m    1712 Hum1, Fem1, Tib1      1644    1712     1678          3
#> 2_21   m    1722 Hum1, Fem1, Tib1      1653    1722     1687          3
#> 2_22   m    1731 Hum1, Fem1, Tib1      1661    1731     1696          3
#> 2_23   m    1740 Hum1, Fem1, Tib1      1669    1740     1705          3
#> 2_24   m    1749 Hum1, Fem1, Tib1      1677    1749     1713          3
#> 2_25   m    1759 Hum1, Fem1, Tib1      1686    1759     1723          3
#> 2_26   m    1768 Hum1, Fem1, Tib1      1694    1768     1731          3
#> 2_27   m    1778 Hum1, Fem1, Tib1      1704    1778     1741          3
#> 2_28   m    1788 Hum1, Fem1, Tib1      1713    1788     1750          3
#> 2_29   m    1797 Hum1, Fem1, Tib1      1721    1797     1759          3
#> 2_3    m    1552 Hum1, Fem1, Tib1      1499    1552     1526          3
#> 2_30   m    1807 Hum1, Fem1, Tib1      1730    1807     1768          3
#> 2_31   m    1815 Hum1, Fem1, Tib1      1737    1815     1776          3
#> 2_32   m    1825 Hum1, Fem1, Tib1      1746    1825     1786          3
#> 2_33   m    1834 Hum1, Fem1, Tib1      1754    1834     1794          3
#> 2_34   m    1844 Hum1, Fem1, Tib1      1763    1844     1804          3
#> 2_35   m    1852 Hum1, Fem1, Tib1      1771    1852     1812          3
#> 2_36   m    1862 Hum1, Fem1, Tib1      1780    1862     1821          3
#> 2_37   m    1872 Hum1, Fem1, Tib1      1789    1872     1830          3
#> 2_38   m    1881 Hum1, Fem1, Tib1      1797    1881     1839          3
#> 2_39   m    1890 Hum1, Fem1, Tib1      1805    1890     1848          3
#> 2_4    m    1562 Hum1, Fem1, Tib1      1508    1562     1535          3
#> 2_40   m    1899 Hum1, Fem1, Tib1      1813    1899     1856          3
#> 2_41   m    1909 Hum1, Fem1, Tib1      1822    1909     1866          3
#> 2_42   m    1918 Hum1, Fem1, Tib1      1830    1918     1874          3
#> 2_43   m    1928 Hum1, Fem1, Tib1      1840    1928     1884          3
#> 2_44   m    1938 Hum1, Fem1, Tib1      1848    1938     1893          3
#> 2_45   m    1947 Hum1, Fem1, Tib1      1857    1947     1902          3
#> 2_46   m    1957 Hum1, Fem1, Tib1      1866    1957     1911          3
#> 2_47   m    1965 Hum1, Fem1, Tib1      1873    1965     1919          3
#> 2_5    m    1572 Hum1, Fem1, Tib1      1517    1572     1544          3
#> 2_6    m    1580 Hum1, Fem1, Tib1      1524    1580     1552          3
#> 2_7    m    1590 Hum1, Fem1, Tib1      1533    1590     1562          3
#> 2_8    m    1599 Hum1, Fem1, Tib1      1541    1599     1570          3
#> 2_9    m    1609 Hum1, Fem1, Tib1      1550    1609     1580          3
#> 3_1    f    1411 Hum1, Fem1, Tib1      1411    1456     1434          3
#> 3_10   f    1477 Hum1, Fem1, Tib1      1477    1528     1502          3
#> 3_11   f    1484 Hum1, Fem1, Tib1      1484    1536     1510          3
#> 3_12   f    1491 Hum1, Fem1, Tib1      1491    1544     1518          3
#> 3_13   f    1498 Hum1, Fem1, Tib1      1498    1552     1525          3
#> 3_14   f    1505 Hum1, Fem1, Tib1      1505    1560     1533          3
#> 3_15   f    1513 Hum1, Fem1, Tib1      1513    1568     1541          3
#> 3_16   f    1521 Hum1, Fem1, Tib1      1521    1576     1549          3
#> 3_17   f    1528 Hum1, Fem1, Tib1      1528    1585     1557          3
#> 3_18   f    1535 Hum1, Fem1, Tib1      1535    1593     1564          3
#> 3_19   f    1543 Hum1, Fem1, Tib1      1543    1601     1572          3
#> 3_2    f    1418 Hum1, Fem1, Tib1      1418    1464     1441          3
#> 3_20   f    1550 Hum1, Fem1, Tib1      1550    1609     1579          3
#> 3_21   f    1558 Hum1, Fem1, Tib1      1558    1617     1587          3
#> 3_22   f    1565 Hum1, Fem1, Tib1      1565    1625     1595          3
#> 3_23   f    1572 Hum1, Fem1, Tib1      1572    1632     1602          3
#> 3_24   f    1579 Hum1, Fem1, Tib1      1579    1641     1610          3
#> 3_25   f    1586 Hum1, Fem1, Tib1      1586    1649     1617          3
#> 3_26   f    1594 Hum1, Fem1, Tib1      1594    1657     1625          3
#> 3_27   f    1601 Hum1, Fem1, Tib1      1601    1665     1633          3
#> 3_28   f    1608 Hum1, Fem1, Tib1      1608    1672     1640          3
#> 3_29   f    1615 Hum1, Fem1, Tib1      1615    1680     1647          3
#> 3_3    f    1425 Hum1, Fem1, Tib1      1425    1471     1448          3
#> 3_30   f    1622 Hum1, Fem1, Tib1      1622    1688     1655          3
#> 3_31   f    1629 Hum1, Fem1, Tib1      1629    1696     1663          3
#> 3_32   f    1636 Hum1, Fem1, Tib1      1636    1704     1670          3
#> 3_33   f    1644 Hum1, Fem1, Tib1      1644    1712     1678          3
#> 3_34   f    1651 Hum1, Fem1, Tib1      1651    1720     1685          3
#> 3_35   f    1658 Hum1, Fem1, Tib1      1658    1728     1693          3
#> 3_36   f    1666 Hum1, Fem1, Tib1      1666    1736     1701          3
#> 3_37   f    1674 Hum1, Fem1, Tib1      1674    1745     1709          3
#> 3_38   f    1681 Hum1, Fem1, Tib1      1681    1753     1717          3
#> 3_39   f    1688 Hum1, Fem1, Tib1      1688    1761     1725          3
#> 3_4    f    1433 Hum1, Fem1, Tib1      1433    1480     1456          3
#> 3_40   f    1695 Hum1, Fem1, Tib1      1695    1769     1732          3
#> 3_41   f    1703 Hum1, Fem1, Tib1      1703    1777     1740          3
#> 3_42   f    1710 Hum1, Fem1, Tib1      1710    1785     1747          3
#> 3_43   f    1717 Hum1, Fem1, Tib1      1717    1793     1755          3
#> 3_44   f    1725 Hum1, Fem1, Tib1      1725    1801     1763          3
#> 3_45   f    1732 Hum1, Fem1, Tib1      1732    1809     1770          3
#> 3_5    f    1440 Hum1, Fem1, Tib1      1440    1488     1464          3
#> 3_6    f    1448 Hum1, Fem1, Tib1      1448    1496     1472          3
#> 3_7    f    1455 Hum1, Fem1, Tib1      1455    1504     1479          3
#> 3_8    f    1462 Hum1, Fem1, Tib1      1462    1512     1487          3
#> 3_9    f    1469 Hum1, Fem1, Tib1      1469    1520     1494          3
#> 4_1    f    1417 Hum1, Fem1, Tib1      1417    1463     1440          3
#> 4_10   f    1490 Hum1, Fem1, Tib1      1490    1543     1516          3
#> 4_11   f    1498 Hum1, Fem1, Tib1      1498    1552     1525          3
#> 4_12   f    1506 Hum1, Fem1, Tib1      1506    1560     1533          3
#> 4_13   f    1515 Hum1, Fem1, Tib1      1515    1570     1543          3
#> 4_14   f    1523 Hum1, Fem1, Tib1      1523    1579     1551          3
#> 4_15   f    1530 Hum1, Fem1, Tib1      1530    1587     1559          3
#> 4_16   f    1539 Hum1, Fem1, Tib1      1539    1596     1567          3
#> 4_17   f    1547 Hum1, Fem1, Tib1      1547    1606     1576          3
#> 4_18   f    1555 Hum1, Fem1, Tib1      1555    1614     1584          3
#> 4_19   f    1563 Hum1, Fem1, Tib1      1563    1623     1593          3
#> 4_2    f    1425 Hum1, Fem1, Tib1      1425    1471     1448          3
#> 4_20   f    1571 Hum1, Fem1, Tib1      1571    1632     1601          3
#> 4_21   f    1580 Hum1, Fem1, Tib1      1580    1642     1611          3
#> 4_22   f    1588 Hum1, Fem1, Tib1      1588    1651     1619          3
#> 4_23   f    1596 Hum1, Fem1, Tib1      1596    1659     1627          3
#> 4_24   f    1604 Hum1, Fem1, Tib1      1604    1668     1636          3
#> 4_25   f    1612 Hum1, Fem1, Tib1      1612    1677     1644          3
#> 4_26   f    1621 Hum1, Fem1, Tib1      1621    1687     1654          3
#> 4_27   f    1628 Hum1, Fem1, Tib1      1628    1695     1662          3
#> 4_28   f    1636 Hum1, Fem1, Tib1      1636    1703     1670          3
#> 4_29   f    1644 Hum1, Fem1, Tib1      1644    1712     1678          3
#> 4_3    f    1433 Hum1, Fem1, Tib1      1433    1480     1457          3
#> 4_30   f    1653 Hum1, Fem1, Tib1      1653    1722     1687          3
#> 4_31   f    1661 Hum1, Fem1, Tib1      1661    1731     1696          3
#> 4_32   f    1669 Hum1, Fem1, Tib1      1669    1739     1704          3
#> 4_33   f    1677 Hum1, Fem1, Tib1      1677    1748     1713          3
#> 4_34   f    1686 Hum1, Fem1, Tib1      1686    1758     1722          3
#> 4_35   f    1694 Hum1, Fem1, Tib1      1694    1767     1730          3
#> 4_36   f    1701 Hum1, Fem1, Tib1      1701    1775     1738          3
#> 4_37   f    1709 Hum1, Fem1, Tib1      1709    1784     1747          3
#> 4_38   f    1718 Hum1, Fem1, Tib1      1718    1794     1756          3
#> 4_39   f    1726 Hum1, Fem1, Tib1      1726    1803     1764          3
#> 4_4    f    1441 Hum1, Fem1, Tib1      1441    1488     1465          3
#> 4_40   f    1734 Hum1, Fem1, Tib1      1734    1811     1773          3
#> 4_41   f    1741 Hum1, Fem1, Tib1      1741    1820     1781          3
#> 4_42   f    1751 Hum1, Fem1, Tib1      1751    1830     1790          3
#> 4_43   f    1758 Hum1, Fem1, Tib1      1758    1838     1798          3
#> 4_44   f    1766 Hum1, Fem1, Tib1      1766    1847     1807          3
#> 4_45   f    1774 Hum1, Fem1, Tib1      1774    1856     1815          3
#> 4_5    f    1449 Hum1, Fem1, Tib1      1449    1498     1474          3
#> 4_6    f    1457 Hum1, Fem1, Tib1      1457    1507     1482          3
#> 4_7    f    1465 Hum1, Fem1, Tib1      1465    1515     1490          3
#> 4_8    f    1473 Hum1, Fem1, Tib1      1473    1524     1499          3
#> 4_9    f    1482 Hum1, Fem1, Tib1      1482    1534     1508          3
#> 
# telkkae_1950(dl.tgb) # The alternative.
```
