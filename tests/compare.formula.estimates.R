# Check formula by differences

compare.formula.bach1965 <- function () {
  # Check formula by differences
  # between provided and calculated stature estimations
  # Read csv data, e.g. Bach1965.csv including stature
  Bach1965 <- read.csv(
    system.file("extdata", "Bach1965.csv", package = "statuAAR"),
    sep = ",",
    header = TRUE,
    skip=3,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  # Prepare tabled data into a long list (statuaar_data_table)
  dl.bach1965 <- statuAAR::prep.statuaar.data(Bach1965, d.form = "wide",
                                              measures.names = "short", sex = "sex", stats = FALSE)

  # Get stature estimations
  #short <- statuAAR::getFormulaDataframe()[,1]
  bb65.estimates <- statuAAR::getStature("all", dl.bach1965)
  bb65.estimates.df <- statuAAR::getStatureDataframe(bb65.estimates)

  # Merge the original published and the calculated stature.
  bb65.check <- merge(Bach1965[5], bb65.estimates.df,
                      by.x = 'row.names', by.y = 'id')
  bb65.check$diff <- bb65.check$stature.x - bb65.check$stature.y

  # Calculate the difference.
  boxplot(data = bb65.check, diff~formula,
          main = "Stature (Bach 1965) and estimates",
          xlab = "Formula abbreviation (short)",
          ylab = "Orig. stature difference (mm)")
}

compare.formula.trottergleser1952 <- function () {
  # Check formula by differences
  # between provided and calculated stature estimations
  # Read csv data, e.g. Trotter&Gleser including stature
  tg52 <- read.csv(
    system.file("extdata", "TrotterGleser1952.csv", package = "statuAAR"),
    sep = ",",
    header = TRUE,
    skip=2,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  # Prepare tabled data into a long list (statuaar_data_table)
  dl.tg52 <- statuAAR::prep.statuaar.data(tg52, d.form = "wide", ind = "Appendix_row",
                            sex = "Sex", measures.names = "own", stats = FALSE)

  # Get stature estimations
  tg52.estimates <- statuAAR::getStature("all", dl.tg52)
  tg52.estimates.df <- statuAAR::getStatureDataframe(tg52.estimates)

  # Merge the original published and the calculated stature.
  tg52.check <- merge(tg52[,c(1,4)], tg52.estimates.df,
                      by.x = 'Appendix_row', by.y = 'id')
  tg52.check$diff <- tg52.check$stature.x - tg52.check$stature.y

  # Calculate the difference.
  boxplot(data = tg52.check, diff~formula,
          main = "Stature (Trotter & Gleser 1952) and estimates",
          xlab = "Formula abbreviation (short)",
          ylab = "Orig. stature difference (mm)")
}
