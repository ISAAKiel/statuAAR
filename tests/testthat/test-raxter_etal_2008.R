test_that("raxter_etal_2008 aggregates left/right values and computes stature correctly", {

  df <- data.frame(
    Ind = c("ind1", "ind1", "ind1", "ind1"),
    Sex = factor(c("m","m","m","m"), levels = c("m","f","indet")),
    variable = c("Fem1l", "Fem1r", "Hum1", "Rad1"),
    value = c(445, 445.4, 312, 240)
  )

  res <- raxter_etal_2008(df)

  expect_s3_class(res, "data.frame")
  expect_equal(rownames(res), "ind1")

  # laterality removed, Fem1 averaged, Tib1 excluded
  expect_equal(res["ind1", "bone"], "Fem1")

  # n_measures counts both Fem1
  expect_equal(res["ind1", "n_measures"], 2)

  # expected values:
  # Stature is caclutaed by the mean of all possible regression culculations.
  # No individual listing here.
  tol <- 1
  expect_true(res["ind1", "if_male"] - 1644 <= tol)
  expect_true(res["ind1", "if_female"] - 1612 <= tol)
  expect_true(res["ind1", "if_indet"] - 1628 <= tol)

  # stature should follow the provided sex
  expect_true(res["ind1", "stature"] - 1644 <= tol)
})

test_that("raxter_etal_2008 returns message when no usable measures are present", {
  df <- data.frame(
    Ind = "ind1",
    Sex = factor(c("m"), levels = c("m","f","indet")),
    variable = "Hum2",
    value = 320
  )

  res <- raxter_etal_2008(df)

  expect_match(
    res,
    "bone measurement"
  )
})
