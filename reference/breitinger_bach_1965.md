# Calculate stature estimation according to Breitinger 1938 & Bach 1965.

Stature estimation (mm) based on the mean of different regression
calculations, separated by sex (Breitinger 1938, Bach 1965). Bone
measures used: Hum2, Hum1, Rad1b, Fem1, Tib1b.

If bone measures for left and right are provided the mean value will be
used, but for statistic information 2 bones will be counted
(n_measures). If sex is indet. the mean of male and female stature
estimation is given. Breitinger (1938) does not show the regression
equation for Hum1, but the equation can be derived from the values given
in the table p. 272 (see Siegmund 2010, p. 112). In contrast, Bach
(1965) gives both equations. In addition, Bach (1965, 20) states to use
only one of the Humeri measures for estimation (Hum1 or Hum2 or the mean
of both) to avoid inadmissable multiplication of the bone within the
stature estimation.

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
breitinger_bach_1965(df)
```

## Arguments

- df:

  data.frame of type statuaar_data_table, containing informations on
  individual, bone and measurement

## Value

data.frame, containing one data.frame with all calculated indices for
every individual

## References

Bach H (1965). “Zur Berechnung der Körperhöhe aus den langen
Gliedmaßenknochen weiblicher Skelette.” *Anthropologischer Anzeiger*,
**29**, 12–21. ISSN 0003-5548.

Breitinger E (1937). “Zur Berechnung der Körperhöhe aus den langen
Gliedmaßenknochen.” *Anthropologischer Anzeiger*, **14**(3/4), 249–274.
ISSN 0003-5548.

Siegmund F (2010). *Die Körpergröße der Menschen in der Ur- und
Frühgeschichte Mitteleuropas und ein Vergleich ihrer anthropologischen
Schätzmethoden*. Books on Demand, Norderstedt. ISBN 978-3-8391-5314-7,
tex.ids: siegmundKorpergrosseMenschenUr2010a.

## Author

Christoph Rinne <crinne@ufg.uni-kiel.de>

## Examples

``` r
# Read example dataset into a data frame
x <- statuAAR::Bach1965

# Prepare tabled data into a long list (statuaar_data_table)
dl.bach1965 <- statuAAR::prep.statuaar.data(Bach1965, d.form = "wide",
                       measures.names = "short", sex = "sex", stats = FALSE)
#> Warning: No individual identifier provided, each record (row) will be counted as one individual.

# Calculate stature estimation using a given formula.
bb65.estimates <- statuAAR::getStature(c("bb65"), dl.bach1965)

# Extract the corresponding data frame from the returned list object.
bb65.estimates[["bb65"]]
#>    sex stature                     bone if_female if_male if_indet n_measures
#> 32   m    1540 Hum2, Rad1b, Fem1, Tib1b      1531    1540     1536          4
#> 33   m    1550 Hum2, Rad1b, Fem1, Tib1b      1538    1550     1544          4
#> 34   m    1560 Hum2, Rad1b, Fem1, Tib1b      1546    1560     1553          4
#> 35   m    1570 Hum2, Rad1b, Fem1, Tib1b      1554    1570     1562          4
#> 36   m    1580 Hum2, Rad1b, Fem1, Tib1b      1561    1580     1571          4
#> 37   m    1590 Hum2, Rad1b, Fem1, Tib1b      1570    1590     1580          4
#> 38   m    1600 Hum2, Rad1b, Fem1, Tib1b      1578    1600     1589          4
#> 39   m    1610 Hum2, Rad1b, Fem1, Tib1b      1585    1610     1597          4
#> 40   m    1620 Hum2, Rad1b, Fem1, Tib1b      1593    1620     1607          4
#> 41   m    1630 Hum2, Rad1b, Fem1, Tib1b      1601    1630     1616          4
#> 42   m    1640 Hum2, Rad1b, Fem1, Tib1b      1608    1640     1624          4
#> 43   m    1650 Hum2, Rad1b, Fem1, Tib1b      1616    1650     1633          4
#> 44   m    1660 Hum2, Rad1b, Fem1, Tib1b      1624    1660     1642          4
#> 45   m    1670 Hum2, Rad1b, Fem1, Tib1b      1632    1670     1651          4
#> 46   m    1680 Hum2, Rad1b, Fem1, Tib1b      1639    1680     1660          4
#> 47   m    1690 Hum2, Rad1b, Fem1, Tib1b      1647    1690     1669          4
#> 48   m    1701 Hum2, Rad1b, Fem1, Tib1b      1656    1701     1678          4
#> 49   m    1710 Hum2, Rad1b, Fem1, Tib1b      1663    1710     1686          4
#> 50   m    1720 Hum2, Rad1b, Fem1, Tib1b      1670    1720     1695          4
#> 51   m    1730 Hum2, Rad1b, Fem1, Tib1b      1678    1730     1704          4
#> 52   m    1740 Hum2, Rad1b, Fem1, Tib1b      1686    1740     1713          4
#> 53   m    1750 Hum2, Rad1b, Fem1, Tib1b      1694    1750     1722          4
#> 54   m    1760 Hum2, Rad1b, Fem1, Tib1b      1702    1760     1731          4
#> 55   m    1770 Hum2, Rad1b, Fem1, Tib1b      1709    1770     1739          4
#> 56   m    1781 Hum2, Rad1b, Fem1, Tib1b      1718    1781     1749          4
#> 57   m    1791 Hum2, Rad1b, Fem1, Tib1b      1726    1791     1758          4
#> 58   m    1800 Hum2, Rad1b, Fem1, Tib1b      1733    1800     1766          4
#> 59   m    1810 Hum2, Rad1b, Fem1, Tib1b      1741    1810     1776          4
#> 60   m    1820 Hum2, Rad1b, Fem1, Tib1b      1749    1820     1785          4
#> 61   m    1830 Hum2, Rad1b, Fem1, Tib1b      1756    1830     1793          4
#> 62   m    1840 Hum2, Rad1b, Fem1, Tib1b      1764    1840     1802          4
#> 63   m    1850 Hum2, Rad1b, Fem1, Tib1b      1772    1850     1811          4
#> 64   m    1861 Hum2, Rad1b, Fem1, Tib1b      1780    1861     1820          4
#> 1    f    1450 Hum2, Rad1b, Fem1, Tib1b      1450    1434     1442          4
#> 10   f    1540 Hum2, Rad1b, Fem1, Tib1b      1540    1550     1545          4
#> 11   f    1550 Hum2, Rad1b, Fem1, Tib1b      1550    1563     1557          4
#> 12   f    1560 Hum2, Rad1b, Fem1, Tib1b      1560    1576     1568          4
#> 13   f    1569 Hum2, Rad1b, Fem1, Tib1b      1569    1588     1579          4
#> 14   f    1580 Hum2, Rad1b, Fem1, Tib1b      1580    1602     1591          4
#> 15   f    1590 Hum2, Rad1b, Fem1, Tib1b      1590    1615     1603          4
#> 16   f    1599 Hum2, Rad1b, Fem1, Tib1b      1599    1627     1613          4
#> 17   f    1610 Hum2, Rad1b, Fem1, Tib1b      1610    1641     1625          4
#> 18   f    1620 Hum2, Rad1b, Fem1, Tib1b      1620    1654     1637          4
#> 19   f    1630 Hum2, Rad1b, Fem1, Tib1b      1630    1667     1648          4
#> 2    f    1459 Hum2, Rad1b, Fem1, Tib1b      1459    1446     1452          4
#> 20   f    1640 Hum2, Rad1b, Fem1, Tib1b      1640    1680     1660          4
#> 21   f    1650 Hum2, Rad1b, Fem1, Tib1b      1650    1693     1671          4
#> 22   f    1660 Hum2, Rad1b, Fem1, Tib1b      1660    1706     1683          4
#> 23   f    1669 Hum2, Rad1b, Fem1, Tib1b      1669    1718     1694          4
#> 24   f    1680 Hum2, Rad1b, Fem1, Tib1b      1680    1731     1705          4
#> 25   f    1690 Hum2, Rad1b, Fem1, Tib1b      1690    1745     1718          4
#> 26   f    1700 Hum2, Rad1b, Fem1, Tib1b      1700    1758     1729          4
#> 27   f    1710 Hum2, Rad1b, Fem1, Tib1b      1710    1770     1740          4
#> 28   f    1720 Hum2, Rad1b, Fem1, Tib1b      1720    1783     1752          4
#> 29   f    1730 Hum2, Rad1b, Fem1, Tib1b      1730    1797     1763          4
#> 3    f    1470 Hum2, Rad1b, Fem1, Tib1b      1470    1459     1464          4
#> 30   f    1740 Hum2, Rad1b, Fem1, Tib1b      1740    1809     1774          4
#> 31   f    1750 Hum2, Rad1b, Fem1, Tib1b      1750    1822     1786          4
#> 4    f    1480 Hum2, Rad1b, Fem1, Tib1b      1480    1472     1476          4
#> 5    f    1489 Hum2, Rad1b, Fem1, Tib1b      1489    1484     1487          4
#> 6    f    1500 Hum2, Rad1b, Fem1, Tib1b      1500    1498     1499          4
#> 7    f    1510 Hum2, Rad1b, Fem1, Tib1b      1510    1511     1510          4
#> 8    f    1520 Hum2, Rad1b, Fem1, Tib1b      1520    1524     1522          4
#> 9    f    1529 Hum2, Rad1b, Fem1, Tib1b      1529    1536     1533          4
```
