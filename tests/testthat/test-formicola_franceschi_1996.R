test_that("formicola_franceschi_1996 aggregates left/right values and computes stature correctly", {

  df <- data.frame(
    Ind = c("ind1", "ind1", "ind1", "ind1"),
    Sex = factor(c("m","m","m","m"), levels = c("m","f","indet")),
    variable = c("Fem1l", "Fem2r", "Fem2l", "Tib1"),
    value = c(450, 458, 455, 380)
  )

  res <- formicola_franceschi_1996(df)

  expect_s3_class(res, "data.frame")
  expect_equal(rownames(res), "ind1")

  # laterality removed, Fem1 averaged, Tib1 excluded
  expect_equal(res["ind1", "bone"], "1. Fem2&Tib1")

  # n_measures counts both Fem1
  expect_equal(res["ind1", "n_measures"], 3)

  # expected values:
  # Mean: mean(c(458, 455)) = 456.5
  # 1. Fem2 + Tib1 = 455 + 380 = 835
  # Male:   ((456.5 + 380) * 1.30) + 604.2 = 1691.65
  # Female: ((456.5 + 380) * 1.33) + 545.7 = 1658.245
  # Indet:  (1691.65 + 1658.245) / 2 = 1674.947
  tol <- 1
  expect_true(res["ind1", "if_male"] - 1691.65 <= tol)
  expect_true(res["ind1", "if_female"] - 1658.245 <= tol)
  expect_true(res["ind1", "if_indet"] - 1674.947 <= tol)

  # stature should follow the provided sex
  expect_true(res["ind1", "stature"] - 1691.65 <= tol)
})

test_that("formicola_franceschi_1996 returns message when no usable measures are present", {
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
