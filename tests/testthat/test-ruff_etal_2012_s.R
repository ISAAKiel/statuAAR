test_that("ruff_etal_2012_s aggregates left/right values and computes stature correctly", {

  df <- data.frame(
    Ind = c("ind1", "ind1", "ind1", "ind1"),
    Sex = factor(c("m","m","m","m"), levels = c("m","f","indet")),
    variable = c("Fem1l", "Fem1r", "Hum1", "Tib1"),
    value = c(445, 445.4, 312, 380)
  )

  res <- ruff_etal_2012_s(df)

  expect_s3_class(res, "data.frame")
  expect_equal(rownames(res), "ind1")

  # laterality removed, Fem1 averaged, others excluded
  expect_equal(res["ind1", "bone"], "Fem1+Tib1")

  # n_measures counts both Fem1
  expect_equal(res["ind1", "n_measures"], 3)

  # expected values:
  # if_male: ((445.2 + 380) * 1.4) + 496.8
  # if_female: ((445.2 + 380) * 1.47) + 429.6
  # if_indet: ((445.2 + 380) * 1.48) + 430
  tol <- 1
  expect_true(res["ind1", "if_male"] - 1652.08 <= tol)
  expect_true(res["ind1", "if_female"] - 1642.644 <= tol)
  expect_true(res["ind1", "if_indet"] - 1651.296 <= tol)

  # stature should follow the provided sex
  expect_true(res["ind1", "stature"] - 1652.08 <= tol)
})

test_that("ruff_etal_2012_s returns message when no usable measures are present", {
  df <- data.frame(
    Ind = "ind1",
    Sex = factor(c("m"), levels = c("m","f","indet")),
    variable = "Hum2",
    value = 320
  )

  res <- ruff_etal_2012_s(df)

  expect_match(
    res,
    "bone measurement"
  )
})
