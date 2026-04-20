test_that("byers_etal_1984 aggregates left/right values and computes stature correctly", {
  df <- data.frame(
    Ind = c("ind1", "ind1", "ind1"),
    Sex = factor(c("m","m","m"), levels = c("m","f","indet")),
    variable = c("MtI1l", "MtI1r", "Tib1"),
    value = c(49, 51, 380)
  )

  res <- byers_etal_1989(df)

  expect_s3_class(res, "data.frame")
  expect_equal(rownames(res), "ind1")

  # laterality removed, Fem1 averaged, Tib1 excluded
  expect_equal(res["ind1", "bone"], "MtI1")

  # n_measures counts both Fem1
  expect_equal(res["ind1", "n_measures"], 2)

  # expected values:
  # MtI1 mean = (49 + 51) / 2 = 50
  # Male:   (50 * 14.3) + 815 = 1530
  # Female: (50 * 13.9) + 783 = 1478
  # Indet:  (50 * 16.8) + 634 = 1474
  expect_equal(res["ind1", "if_male"], 1530)
  expect_equal(res["ind1", "if_female"], 1478)
  expect_equal(res["ind1", "if_indet"], 1474)

  # stature should follow the provided sex
  expect_equal(res["ind1", "stature"], 1530)
})

test_that("byers_etal_1989 returns message when no usable measures are present", {
  df <- data.frame(
    Ind = "ind1",
    Sex = 1,
    variable = "Hum1",
    value = 320
  )

  res <- byers_etal_1989(df)

  expect_match(
    res,
    "bone measurement"
  )
})
