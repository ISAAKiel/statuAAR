test_that("pearson_1899 aggregates left/right values and computes stature correctly", {

  df <- data.frame(
    Ind = c("ind1", "ind1", "ind1", "ind1"),
    Sex = factor(c("m","m","m","m"), levels = c("m","f","indet")),
    variable = c("Fem1l", "Fem1r", "Hum1", "Rad1"),
    value = c(445, 445.4, 312, 240)
  )

  res <- pearson_1899(df)

  expect_s3_class(res, "data.frame")
  expect_equal(rownames(res), "ind1")

  # laterality removed, Fem1 averaged, Tib1 excluded
  expect_equal(res["ind1", "bone"], "Hum1, Rad1, Fem1")

  # n_measures counts both Fem1
  expect_equal(res["ind1", "n_measures"], 4)

  # expected values:
  # Stature is caclutaed by the mean of all possible regression culculations.
  # No individual listing here.
  tol <- 1
  expect_true(res["ind1", "if_male"] - 1627 <= tol)
  expect_true(res["ind1", "if_female"] - 1592 <= tol)
  expect_true(res["ind1", "if_indet"] - 1610 <= tol)

  # stature should follow the provided sex
  expect_true(res["ind1", "stature"] - 1627 <= tol)
})

test_that("pearson_1899 returns message when no usable measures are present", {
  df <- data.frame(
    Ind = "ind1",
    Sex = factor(c("m"), levels = c("m","f","indet")),
    variable = "Hum2",
    value = 320
  )

  res <- pearson_1899(df)

  expect_match(
    res,
    "bone measurement"
  )
})
