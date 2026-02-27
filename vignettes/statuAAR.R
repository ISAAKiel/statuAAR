## ----setup, include=FALSE-----------------------------------------------------
library(statuAAR)
knitr::opts_chunk$set(echo = TRUE)

## ----get formulas-------------------------------------------------------------
statuAAR::getFormulaDataframe()

## ----check B & B--------------------------------------------------------------
?statuAAR::breitinger_bach_1965

## ----read B & B---------------------------------------------------------------
Bach1965 <- statuAAR::Bach1965
head(Bach1965)

## ----prep B & B---------------------------------------------------------------
dl.bach1965 <- statuAAR::prep.statuaar.data(Bach1965, d.form = "wide",
                   measures.names = "short", sex = "sex", stats = FALSE)

## ----check mesaurements-------------------------------------------------------
statuAAR::measures.statistics(dl.bach1965)

## ----get estimates------------------------------------------------------------
head(statuAAR::getStature(c('bb65'), dl.bach1965)$bb65)

