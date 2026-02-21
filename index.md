# statuAAR

A R package to calculate human stature estimation from bone
measurements.

With `statuAAR` you can calculate human stature estimations according to
several well established formula based on measured bone length. All you
need is a table with individuals in rows and measurements in columns, or
a repeated individual ID and one measurement per row. Different formulas
can be selected for each data set, e.g. Breitinge & Bach 1956, Pearson
1899 or Ruff et al. 2012, and the correspondig results will be provided
as a list with one data frame for each formula.

To date, a large number of formulas have been developed for various bone
measurements and reference populations, yielding a wide range of
possible results. The quality of the result depends mainly on the
representativeness of the original sample used in the formula in
relation to the data collection at hand. A straightforward and quick
calculation using different formulas allows for easy comparison of the
different results.

A general introduction to human stature estimation based on bone
measurements can be found, for example, in

- **Ruff, Chr. B. (ed.) (2018).** Skeletal variation and adaptation in
  Europeans: upper Paleolithic to the Twentieth Century. Hoboken, NJ:
  John Wiley & Sons.

- **Siegmund, F. (2019).** Die Körpergröße der Menschen in der Ur- und
  Frühgeschichte Mitteleuropas und ein Vergleich ihrer anthropologischen
  Schätzmethoden. (Beiträge zur Archäologie des Lebensstandards, 1).
  Norderstedt: BoD.

We provide a description for each formula, including required or
possible bone measurements and citations (s. bibliography file
[BibTeX](https://isaakiel.github.io/statuAAR/inst/REFERENCES.bib)). Bone
measurements are designated according to:

- **Martin, R. (1928).** Lehrbuch der Anthropologie in systematischer
  Darstellung: mit besonderer Berücksichtigung der anthropologischen
  Methoden / für Studierende, Ärzte und Forschungsreisende. 3 vols.
  Fischer: Jena.

The package is still under development, but the basic workflow of data
preparation, verification and calculation is possible. Explanatory
vignettes are in preparation.

## How to cite this package

You can cite this package like this: “we calculated human stature values
with the statuAAR R package (Rinne et al. 2026)”. Here is the full
bibliographic reference to include in your reference list (don’t forget
to update the ‘last accessed’ date):

> C. Rinne, N. Müller-Scheeßel, H. Raese (2026). *statuAAR: Human
> stature estimation from bone measures based on multiple formula*. R
> package version 0.0.9000, <https://github.com/ISAAKiel/statuAAR>

## Installation

`statuAAR` is currently not on [CRAN](http://cran.r-project.org/), but
you can use
[devtools](http://cran.r-project.org/web/packages/devtools/index.md) to
install the development version. To do so:

``` R
if(!require('devtools')) install.packages('devtools')
library(devtools)
install_github('ISAAKiel/statuAAR')
```

## Quick start

Get a df of the available formula and measures

``` R
statuAAR::getFormulaDataframe()
```

Check Breitinger & Bach 1965 documentation

``` R
?statuAAR::breitinger_bach_1965
```

Read csv data, e.g. Bach1965.csv into data.frame

``` R
statuAAR::Bach1965 <- read.csv(
    "data-raw/Bach1965.csv",
    sep = ",",
    header = TRUE,
    skip=3,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
```

Prepare tabled data into a long list (statuaar_data_table)

``` R
dl.bach1965 <- statuAAR::prep.statuaar.data(Bach1965, d.form = "wide",
               measures.names = "short", sex = "sex", stats = FALSE)
```

Check the measurements using general statistics.

``` R
statuAAR::measures.statistics(dl.bach1965)
```

| measure |  n  | MinM | Quart1 | MedianM | MeanM | Quart3 | MaxM |
|:-------:|:---:|:----:|:------:|:-------:|:-----:|:------:|:----:|
|  Fem1   | 64  | 292  |  382   |   436   |  434  |  490   | 557  |
|  Hum1   | 64  | 220  |  278   |   311   |  309  |  343   | 387  |

Get the stature estimations of the provided measurements. The function
returns a list with a data frame for each formula.

``` R
statuAAR::getStature(c('bb65'), dl.bach1965)
```

\$bb65

| id  | sex | stature |           bone           | female | male | indet | n_measures |
|:---:|:---:|:-------:|:------------------------:|:------:|:----:|:-----:|:----------:|
| 32  |  m  |  1540   | Hum2, Rad1b, Fem1, Tib1b |  1531  | 1540 | 1536  |     4      |
| 33  |  m  |  1550   | Hum2, Rad1b, Fem1, Tib1b |  1538  | 1550 | 1544  |     4      |
| 34  |  m  |  1560   | Hum2, Rad1b, Fem1, Tib1b |  1546  | 1560 | 1553  |     4      |

## Licence

`statuAAR` is released under the [GNU General Public Licence, version
3](http://www.r-project.org/Licenses/GPL-3). Comments and feedback are
welcome, as are code contributions.
