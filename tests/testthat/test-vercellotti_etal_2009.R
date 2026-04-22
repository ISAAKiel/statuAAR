test_that("vercellotti_etal_2009 aggregates left/right values and computes stature correctly", {

  df <- data.frame(
    Ind = c("ind1", "ind1", "ind1", "ind1"),
    Sex = factor(c("m","m","m","m"), levels = c("m","f","indet")),
    variable = c("Fem1l", "Fem1r", "Hum1", "Rad1"),
    value = c(445, 445.4, 312, 240)
  )

  res <- vercellotti_etal_2009(df)

  expect_s3_class(res, "data.frame")
  expect_equal(rownames(res), "ind1")

  # laterality removed, Fem1 averaged, others excluded
  expect_equal(res["ind1", "bone"], "Fem1")

  # n_measures counts both Fem1
  expect_equal(res["ind1", "n_measures"], 2)

  # expected values:
  # if_male: (445.2 * 2.61) + 515 = 1676.972
  # if_female: (445.2 * 2.89) + 353 = 1639.628
  # if_indet: (445.2 * 3.10) + 281 = 1661.12
  tol <- 1
  expect_true(res["ind1", "if_male"] - 1676.972 <= tol)
  expect_true(res["ind1", "if_female"] - 1639.628 <= tol)
  expect_true(res["ind1", "if_indet"] - 1661.12 <= tol)

  # stature should follow the provided sex
  expect_true(res["ind1", "stature"] - 1676.972 <= tol)
})

test_that("vercellotti_etal_2009 returns message when no usable measures are present", {
  df <- data.frame(
    Ind = "ind1",
    Sex = factor(c("m"), levels = c("m","f","indet")),
    variable = "Hum2",
    value = 320
  )

  res <- vercellotti_etal_2009(df)

  expect_match(
    res,
    "bone measurement"
  )
})
