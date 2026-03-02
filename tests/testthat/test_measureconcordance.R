library(testthat)

test_that("Daten korrekt geladen", {
  measures.concordance <- read.table(system.file("extdata","measures.concordance.rollet1888.tab",
                                               package = "statuAAR"))

  testthat::expect_true(!is.null(measures.concordance))
  testthat::expect_true(nrow(measures.concordance) > 0)  # Überprüfe, ob die Datei nicht leer ist
})
