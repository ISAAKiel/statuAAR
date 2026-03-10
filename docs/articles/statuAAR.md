# Introduction to statuAAR

## Qickstart with a general workflow.

Obtain a data frame of the available formulas and measurements and
decide on one or more formulas later.

``` r

knitr::kable(statuAAR::getFormulaDataframe())
```

| short | long | measures |
|:---|:---|:---|
| bb65 | breitinger_bach_1965 | Hum2, Hum1, Rad1b, Fem1, Tib1b |
| by89 | byers_etal_1989 | MtI1 |
| fe90 | feldesman_etal_1990 | Fem1 |
| ff96 | formicola_franceschi_1996 | Fem2+Tib1, Fem1, Tib1, Hum1, Rad1 |
| mn09 | maijanen_niskanen_2009 | Fem2+Tib1, Fem1+Tib1, Fem2, Fem1, Tib1, Hum1, Rad1, Fib1, Uln1 |
| ol78 | olivier_etal_1978 | Hum2, Hum1, Rad1b, Fem2, Tib1b |
| pe99 | pearson_1899 | Hum1, Rad1, Fem1, Tib1b, Tib1a |
| ra08 | raxter_etal_2008 | Fem1, Fem1+Tib1a, Fem2, Fem2+Tib1a, Fem2+Tib1b, Hum1, Hum1+Rad1, Rad1, Tib1a, Tib1b |
| sj90 | sjovold_1990 | Hum1, Rad1, Rad1b, Uln1, Fem1, Fem2, Tib1, Tib1b, Fib1 |
| te50 | telkkae_1950 | Hum1, Rad2, Uln2, Fem1, Tib1, Fib1 |
| tg01 | trotter_gleser_1952_an | Fem1, Tib1b, Fib1, Uln1, Rad1, Hum1 |
| tg02 | trotter_gleser_1952_aw | Fem1, Tib1b, Fib1, Uln1, Rad1, Hum1 |
| ve09 | vercellotti_etal_2009 | Fem2+Tib1, Fem2, Fem1, Tib1, Hum1+Rad1, Hum1, Rad1 |
| r12a | ruff_etal_2012_a | Fem1, Hum1, Rad1 |
| r12s | ruff_etal_2012_s | Fem1+Tib1, Fem1, Tib1 |
| r12n | ruff_etal_2012_n | Fem1+Tib1, Fem1, Tib1 |
| bo84 | boldsen_1984 | Fem1, Tib1 |

You may check the corresponding documentation, e.g. for
breitinger_bach_1965 (Bach 1965).

``` r

?statuAAR::breitinger_bach_1965
```

Load some data into a data frame (e.g. from Bach 1965) .

``` r

Bach1965 <- statuAAR::Bach1965
knitr::kable(head(Bach1965))
```

| sex | Hum1 | Hum2 | Rad1b | stature | Fem1 | Tib1b |
|:----|-----:|-----:|------:|--------:|-----:|------:|
| f   |  220 |  215 |   146 |    1450 |  292 |   281 |
| f   |  224 |  219 |   151 |    1460 |  299 |   287 |
| f   |  229 |  224 |   156 |    1470 |  307 |   293 |
| f   |  233 |  229 |   162 |    1480 |  315 |   298 |
| f   |  239 |  234 |   167 |    1490 |  322 |   301 |
| f   |  243 |  238 |   172 |    1500 |  330 |   310 |

Prepare tabled data into a long list (statuaar_data_table) and accept
the warning about the missing ID for each individual.

``` r

dl.bach1965 <- statuAAR::prep.statuaar.data(Bach1965, d.form = "wide",
                   measures.names = "short", sex = "sex", stats = FALSE)
```

    ## Warning in statuAAR::prep.statuaar.data(Bach1965, d.form = "wide",
    ## measures.names = "short", : No individual identifier provided, each record
    ## (row) will be counted as one individual.

Check the measurements using the function provided for general
statistics.

``` r

statuAAR::measures.statistics(dl.bach1965)
```

    ## # A tibble: 5 × 8
    ##   measure     n  MinM Quart1 MedianM MeanM Quart3  MaxM
    ##   <chr>   <dbl> <dbl>  <dbl>   <dbl> <dbl>  <dbl> <dbl>
    ## 1 Fem1       64   292    382     436   434    490   557
    ## 2 Hum1       64   220    278     311   309    343   387
    ## 3 Hum2       64   215    272     304   303    337   379
    ## 4 Rad1b      64   146    204     238   235    270   302
    ## 5 Tib1b      64   281    328     371   371    414   455

Get the stature estimations of the provided measurements. The function
returns a list with a data frame for each formula. You can address each
data frame with the short name of the corresponding formula.

``` r

knitr::kable(head(statuAAR::getStature(c('bb65'), dl.bach1965)$bb65))
```

|     | sex | stature | bone                     | if_female | if_male | if_indet | n_measures |
|:----|:----|--------:|:-------------------------|----------:|--------:|---------:|-----------:|
| 32  | m   |    1540 | Hum2, Rad1b, Fem1, Tib1b |      1531 |    1540 |     1536 |          4 |
| 33  | m   |    1550 | Hum2, Rad1b, Fem1, Tib1b |      1538 |    1550 |     1544 |          4 |
| 34  | m   |    1560 | Hum2, Rad1b, Fem1, Tib1b |      1546 |    1560 |     1553 |          4 |
| 35  | m   |    1570 | Hum2, Rad1b, Fem1, Tib1b |      1554 |    1570 |     1562 |          4 |
| 36  | m   |    1580 | Hum2, Rad1b, Fem1, Tib1b |      1561 |    1580 |     1571 |          4 |
| 37  | m   |    1590 | Hum2, Rad1b, Fem1, Tib1b |      1570 |    1590 |     1580 |          4 |

## Basic minimal workflow

Create a data frame with measures for one individual per row. If you do
not provide an individual identifier or a sex assignment, row count and
indet. will be added in the subsequent data preparation. We create a
minimal data frame with data for a Neanderthal Man taken from Pearson
(1899, 205).

``` r

x <- data.frame(Fem1 = 445.2, Hum1 = 312, Rad1 = 240)
knitr::kable(x)
```

|  Fem1 | Hum1 | Rad1 |
|------:|-----:|-----:|
| 445.2 |  312 |  240 |

The column names correspond to the abbreviations used for the bone
measurements, so no concordance for the names is required
(create.measures.concordance()). The data can be transfered the wide
table into the long format with only one measure per row
(prep.statuaar.data()). Furthermore, we do not opt for a general
statistic to check for possible errors in the data provided.

``` r

dl.Neanderthal <- statuAAR::prep.statuaar.data(x, d.form = "wide", stat = FALSE)
```

    ## Warning in statuAAR::prep.statuaar.data(x, d.form = "wide", stat = FALSE): No
    ## individual identifier provided, each record (row) will be counted as one
    ## individual.

``` r

knitr::kable(dl.Neanderthal)
```

| Ind | Sex   | variable | value |
|:----|:------|:---------|------:|
| 1   | indet | Fem1     | 445.2 |
| 1   | indet | Hum1     | 312.0 |
| 1   | indet | Rad1     | 240.0 |

We now can calculate the estimated stature in various ways.

1.  Just use a specific function and get a data frame with the estimated
    stature, the bone measurements used and the alternatives:

``` r

pearson_1899(dl.Neanderthal)
```

    ##     sex stature             bone if_female if_male if_indet n_measures
    ## 1 indet    1610 Hum1, Rad1, Fem1      1592    1627     1610          3

2.  You can use the function getStature() and provide a vector of
    formula short names and the prepared data frame to get a list
    containing the corresponding data frames. You can convert the list
    into a single data frame with an additional column for the formula.

``` r

res.Neanderthal <- getStature(c('bo84','bb65','mn09'), dl.Neanderthal)
knitr::kable(getStatureDataframe(res.Neanderthal))
```

|     | formula | id  | sex   | stature | bone       | if_female | if_male | if_indet | n_measures |
|:----|:--------|:----|:------|--------:|:-----------|----------:|--------:|---------:|-----------:|
| 1   | bo84    | 1   | indet |    1642 | Fem1       |      1633 |    1650 |     1642 |          1 |
| 11  | bb65    | 1   | indet |    1658 | Hum1, Fem1 |      1648 |    1667 |     1658 |          2 |
| 12  | mn09    | 1   | indet |    1641 | 4\. Fem1   |      1638 |    1643 |     1641 |          1 |

If you want to get all possible stature estimations use “all” instead of
the vector with specific abbreviations.

``` r

res.Neanderthal <- getStature('all', dl.Neanderthal)
knitr::kable(getStatureDataframe(res.Neanderthal))
```

|  | formula | id | sex | stature | bone | if_female | if_male | if_indet | n_measures |
|:---|:---|:---|:---|---:|:---|---:|---:|---:|---:|
| 1 | bb65 | 1 | indet | 1658 | Hum1, Fem1 | 1648 | 1667 | 1658 | 2 |
| 11 | fe90 | 1 | indet | 1665 | Fem1 | 1665 | 1665 | 1665 | 1 |
| 12 | ff96 | 1 | indet | 1639 | 2\. Fem1 | 1622 | 1656 | 1639 | 1 |
| 13 | mn09 | 1 | indet | 1641 | 4\. Fem1 | 1638 | 1643 | 1641 | 1 |
| 14 | ol78 | 1 | indet | 1612 | Hum1.l, Hum1.l | 1587 | 1637 | 1612 | 2 |
| 15 | pe99 | 1 | indet | 1610 | Hum1, Rad1, Fem1 | 1592 | 1627 | 1610 | 3 |
| 16 | ra08 | 1 | indet | 1628 | Fem1, Fem1 | 1612 | 1644 | 1628 | 2 |
| 17 | sj90 | 1 | indet | 1650 | Hum1, Rad1, Fem1 | 1650 | 1650 | 1650 | 3 |
| 18 | te50 | 1 | indet | 1610 | Hum1, Fem1 | 1579 | 1640 | 1610 | 2 |
| 19 | tg01 | 1 | indet | 1628 | 4\. Fem1 | 1613 | 1643 | 1628 | 1 |
| 110 | tg02 | 1 | indet | 1657 | 4\. Fem1 | 1641 | 1674 | 1657 | 1 |
| 111 | ve09 | 1 | indet | 1661 | Fem1 | 1640 | 1677 | 1661 | 1 |
| 112 | r12a | 1 | indet | 1638 | Fem1 | 1633 | 1639 | 1638 | 1 |
| 113 | r12s | 1 | indet | NA | Tib1 | NA | NA | NA | NA |
| 114 | r12n | 1 | indet | NA | Tib1 | NA | NA | NA | NA |
| 115 | bo84 | 1 | indet | 1642 | Fem1 | 1633 | 1650 | 1642 | 1 |

## References

Bach, H. 1965. “Zur Berechnung Der Körperhöhe Aus Den Langen
Gliedmaßenknochen Weiblicher Skelette.” *Anthropologischer Anzeiger* 29:
12–21.

Pearson, Karl. 1899. “IV. Mathematical Contributions to the Theory of
Evolution.—v. On the Reconstruction of the Stature of Prehistoric
Races.” *Philosophical Transactions of the Royal Society of London.
Series A, Containing Papers of a Mathematical or Physical Character*
192: 169–244. <https://doi.org/10.1098/rsta.1899.0004>.
