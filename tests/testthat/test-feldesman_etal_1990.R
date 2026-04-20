test_that("feldesman_etal_1990 aggregates left/right values and computes stature correctly", {
  df <- data.frame(
    Ind = c("OH28", "OH28", "OH28"),
    Sex = factor(c("m","m","m"), levels = c("m","f","indet")),
    variable = c("Fem1l", "Fem1r", "Tib1"),
    value = c(456, 456, 380)
  )

  res <- feldesman_etal_1990(df)

  expect_s3_class(res, "data.frame")
  expect_equal(rownames(res), "OH28")

  # laterality removed, Fem1 averaged, Tib1 excluded
  expect_equal(res["OH28", "bone"], "Fem1")

  # n_measures counts both Fem1
  expect_equal(res["OH28", "n_measures"], 2)

  # expected values:
  # all:   (456 * 100) / 26.74 = 1705.31
  tol <- 1
  expect_true(res["OH28", "if_male"] - 1705 <= tol)
  expect_true(res["OH28", "if_female"] - 1705 <= tol)
  expect_true(res["OH28", "if_indet"] - 1705 <= tol)

  # stature should follow the provided sex
  expect_true(res["OH28", "stature"] - 1705 <= tol)
})

test_that("feldesman_etal_1990 returns message when no usable measures are present", {
  df <- data.frame(
    Ind = "ind1",
    Sex = factor(c("m"), levels = c("m","f","indet")),
    variable = "Hum1",
    value = 320
  )

  res <- feldesman_etal_1990(df)

  expect_match(
    res,
    "bone measurement"
  )
})
