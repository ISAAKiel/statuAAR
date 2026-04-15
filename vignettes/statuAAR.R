## ----setup, include=FALSE-----------------------------------------------------
library(statuAAR)
#library(tibble)
knitr::opts_chunk$set(echo = TRUE)

## ----get formulas-------------------------------------------------------------
knitr::kable(statuAAR::getFormulaDataframe())

## ----check B & B--------------------------------------------------------------
?statuAAR::breitinger_bach_1965

## ----read B & B---------------------------------------------------------------
Bach1965 <- statuAAR::Bach1965
knitr::kable(head(Bach1965))

## ----prep B & B---------------------------------------------------------------
dl.bach1965 <- statuAAR::prep.statuaar.data(Bach1965, d.form = "wide",
                   measures.names = "short", sex = "sex", stats = FALSE)

## ----check mesaurements B & B-------------------------------------------------
statuAAR::measures.statistics(dl.bach1965)

## ----get estimates B & B------------------------------------------------------
knitr::kable(head(statuAAR::getStature(c('bb65'), dl.bach1965)$bb65))

## ----minimal workflow data entry----------------------------------------------
x <- data.frame(Fem1 = 445.2, Hum1 = 312, Rad1 = 240)
knitr::kable(x)

## ----minimal worflow data preparation-----------------------------------------
dl.Neanderthal <- statuAAR::prep.statuaar.data(x, d.form = "wide", stat = FALSE)
knitr::kable(dl.Neanderthal)

## ----minimal worflow stature pearson_1899-------------------------------------
pearson_1899(dl.Neanderthal)

## ----minimal worflow staure from vector---------------------------------------
res.Neanderthal <- getStature(c('bo84','bb65','mn09'), dl.Neanderthal)
knitr::kable(getStatureDataframe(res.Neanderthal))

## ----minimal worflow staure for all-------------------------------------------
res.Neanderthal <- getStature('all', dl.Neanderthal)
knitr::kable(getStatureDataframe(res.Neanderthal))

