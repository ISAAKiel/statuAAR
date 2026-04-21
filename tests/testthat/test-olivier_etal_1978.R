test_that("olivier_etal_1978 aggregates left/right values and computes stature correctly", {

  df <- data.frame(
    Ind = c("ind1", "ind1", "ind1", "ind1"),
    Sex = factor(c("m","m","m","m"), levels = c("m","f","indet")),
    variable = c("Fem1l", "Fem2r", "Fem2l", "Tib1"),
    value = c(450, 458, 455, 380)
  )

  res <- olivier_etal_1978(df)

  expect_s3_class(res, "data.frame")
  expect_equal(rownames(res), "ind1")

  # laterality removed, Fem1 averaged, Tib1 excluded
  expect_equal(res["ind1", "bone"], "Fem2.rl")

  # n_measures counts both Fem1
  expect_equal(res["ind1", "n_measures"], 2)

  # expected values:
  # Mean: mean(c(458, 455)) = 456.5
  # 1. Fem2 + Tib1 = 456.5 + 380 = 836.5
  # Male:   (456.5 * 2.4184) + 585.05 = 1689.05
  # Female: (456.5 * 2.0960) + 702.0 = 1658.824
  # Indet:  (1689.05 + 1658.824) / 2 = 1673.937
  tol <- 1
  expect_true(res["ind1", "if_male"] - 1689.05 <= tol)
  expect_true(res["ind1", "if_female"] - 1658.824 <= tol)
  expect_true(res["ind1", "if_indet"] - 1673.937 <= tol)

  # stature should follow the provided sex
  expect_true(res["ind1", "stature"] - 1689.05 <= tol)
})

test_that("olivier_etal_1978 returns message when no usable measures are present", {
  df <- data.frame(
    Ind = "ind1",
    Sex = factor(c("m"), levels = c("m","f","indet")),
    variable = "Hum2",
    value = 320
  )

  res <- olivier_etal_1978(df)

  expect_match(
    res,
    "bone measurement"
  )
})
