[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![Coverage Status](https://img.shields.io/codecov/c/github/ISAAKiel/statuAAR/master.svg)](https://app.codecov.io/github/ISAAKiel/statuAAR?branch=master)
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/statuAAR)](https://CRAN.R-project.org/package=statuAAR)
[![](https://cranlogs.r-pkg.org/badges/statuAAR)](https://CRAN.R-project.org/package=statuAAR)
[![](https://cranlogs.r-pkg.org/badges/grand-total/statuAAR)](https://CRAN.R-project.org/package=statuAAR)
[![license](https://img.shields.io/badge/license-GPL%203-B50B82.svg)](https://www.r-project.org/Licenses/GPL-3)

# statuAAR

A R package to calculate human stature estimation from bone measurements.

With `statuAAR` you can calculate human stature estimations according to several well established formula based on measured bone length. All you need is a table with individuals in rows and measurements in columns, or a repeated individual ID and one measurement per row. Different formulas can be selected for each data set, e.g. Breitinge & Bach 1956, Pearson 1899 or Ruff et al. 2012, and the correspondig results will be provided as a list with one data frame for each formula.

To date, a large number of formulas have been developed for various bone measurements and reference populations, yielding a wide range of possible results. The quality of the result depends mainly on the representativeness of the original sample used in the formula in relation to the data collection at hand. A straightforward and quick calculation using different formulas allows for easy comparison of the different results.

A general introduction to human stature estimation based on bone measurements can be found, for example, in

- **Ruff, Chr. B. (ed.) (2018).** Skeletal variation and adaptation in Europeans: upper Paleolithic to the Twentieth Century. Hoboken, NJ: John Wiley & Sons.

- **Siegmund, F. (2019).** Die Körpergröße der Menschen in der Ur- und Frühgeschichte Mitteleuropas und ein Vergleich ihrer anthropologischen Schätzmethoden. (Beiträge zur Archäologie des Lebensstandards, 1). Norderstedt: BoD.

We provide a description for each formula, including required or possible bone measurements and citations (s. bibliography file [BibTeX](./inst/REFERENCES.bib)). Bone measurements are designated according to:

- **Martin, R. (1928).** Lehrbuch der Anthropologie in systematischer Darstellung: mit besonderer Berücksichtigung der anthropologischen Methoden / für Studierende, Ärzte und Forschungsreisende. 3 vols. Fischer: Jena.

The package is still under development, but the basic workflow of data preparation, verification and calculation is possible. Explanatory vignettes are in preparation.

Installation
------------

`statuAAR` is currently not on [CRAN](http://cran.r-project.org/), but you can use [devtools](http://cran.r-project.org/web/packages/devtools/index.html) to install the development version. To do so:

    if(!require('devtools')) install.packages('devtools')
    library(devtools)
    install_github('ISAAKiel/statuAAR')

Licence
-------

`statuAAR` is released under the [GNU General Public Licence, version 3](http://www.r-project.org/Licenses/GPL-3). Comments and feedback are welcome, as are code contributions.
