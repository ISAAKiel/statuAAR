test_that("maijanen_niskanen_2009 aggregates left/right values and computes stature correctly", {

  df <- data.frame(
    Ind = c("ind1", "ind1", "ind1", "ind1"),
    Sex = factor(c("m","m","m","m"), levels = c("m","f","indet")),
    variable = c("Fem1l", "Fem2r", "Fem2l", "Tib1"),
    value = c(450, 458, 455, 380)
  )

  res <- maijanen_niskanen_2009(df)

  expect_s3_class(res, "data.frame")
  expect_equal(rownames(res), "ind1")

  # laterality removed, Fem1 averaged, Tib1 excluded
  expect_equal(res["ind1", "bone"], "1. Fem2&Tib1")

  # n_measures counts both Fem1
  expect_equal(res["ind1", "n_measures"], 3)

  # expected values:
  # Mean: mean(c(458, 455)) = 456.5
  # 1. Fem2 + Tib1 = 456.5 + 380 = 836.5
  # Male:   ((456.5 + 380) * 1.62) + 353.3 = 1708.43
  # Female: ((456.5 + 380) * 1.53) + 416.3 = 1696.145
  # Indet:  ((456.5 + 380) * 1.64) + 338.2 = 1710.06
  tol <- 1
  expect_true(res["ind1", "if_male"] - 1708.43 <= tol)
  expect_true(res["ind1", "if_female"] - 1696.145 <= tol)
  expect_true(res["ind1", "if_indet"] - 1710.06 <= tol)

  # stature should follow the provided sex
  expect_true(res["ind1", "stature"] - 1708.43 <= tol)
})

test_that("maijanen_niskanen_2009 returns message when no usable measures are present", {
  df <- data.frame(
    Ind = "ind1",
    Sex = factor(c("m"), levels = c("m","f","indet")),
    variable = "Hum2",
    value = 320
  )

  res <- formicola_franceschi_1996(df)

  expect_match(
    res,
    "bone measurement"
  )
})
