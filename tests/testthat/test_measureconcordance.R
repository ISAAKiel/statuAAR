library(testthat)

test_that("Daten korrekt geladen", {
  measures.concordance <- read.csv(system.file("data-raw/measures.concordance.rollet1888.csv",
                                               package = "stuAAR"))

  expect_true(!is.null(measures.concordance))
  expect_true(nrow(measures.concordance) > 0)  # Überprüfe, ob die Datei nicht leer ist
})
