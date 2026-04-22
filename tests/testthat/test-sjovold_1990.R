test_that("sjovold_1990 aggregates left/right values and computes stature correctly", {

  df <- data.frame(
    Ind = c("ind1", "ind1", "ind1", "ind1"),
    Sex = factor(c("m","m","m","m"), levels = c("m","f","indet")),
    variable = c("Fem1l", "Fem1r", "Hum1", "Tib1"),
    value = c(445, 445.4, 312, 380)
  )

  res <- sjovold_1990(df)

  expect_s3_class(res, "data.frame")
  expect_equal(rownames(res), "ind1")

  # laterality removed, Fem1 averaged, others excluded
  expect_equal(res["ind1", "bone"], "Hum1, Fem1")

  # n_measures counts both Fem1
  expect_equal(res["ind1", "n_measures"], 3)

  # expected values:
  # Hum1: (4.62 * 312) + 190.00 = 1631.44
  # Fem1: (2.71 * 445.2) + 458.6 = 1665.092
  # mean of both: (1631.44 + 1665.092) / 2 = 1648.266
  tol <- 1
  expect_true(res["ind1", "if_male"] - 1648.266 <= tol)
  expect_true(res["ind1", "if_female"] - 1648.266 <= tol)
  expect_true(res["ind1", "if_indet"] - 1648.266 <= tol)

  # stature should follow the provided sex
  expect_true(res["ind1", "stature"] - 1648.266 <= tol)
})

test_that("sjovold_1990 returns message when no usable measures are present", {
  df <- data.frame(
    Ind = "ind1",
    Sex = factor(c("m"), levels = c("m","f","indet")),
    variable = "Hum2",
    value = 320
  )

  res <- sjovold_1990(df)

  expect_match(
    res,
    "bone measurement"
  )
})
