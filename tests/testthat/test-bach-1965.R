test_that("Bach1965 stature estimates reproduce published statures", {
  Bach1965 <- read.csv(
    system.file("extdata", "Bach1965.csv", package = "statuAAR"),
    sep = ",",
    header = TRUE,
    skip = 3,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  dl.bach1965 <- statuAAR::prep.statuaar.data(
    Bach1965,
    d.form = "wide",
    measures.names = "short",
    sex = "sex",
    stats = FALSE
  )

  bb65.estimates <- statuAAR::getStature("bb65", dl.bach1965)
  bb65.estimates.df <- statuAAR::getStatureDataframe(bb65.estimates)

  bb65.check <- merge(
    Bach1965[5],
    bb65.estimates.df,
    by.x = "row.names",
    by.y = "id"
  )

  bb65.check$diff <- bb65.check$stature.x - bb65.check$stature.y

  # Expect exact agreement, or change tolerance if rounding differences occur
  expect_equal(
    bb65.check$diff,
    rep(0, nrow(bb65.check)),
    tolerance = 1
  )
})
