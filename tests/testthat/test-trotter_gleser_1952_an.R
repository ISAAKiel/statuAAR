test_that("trotter_gleser_1952_an aggregates left/right values and computes stature correctly", {

  df <- data.frame(
    Ind = c("ind1", "ind1", "ind1", "ind1"),
    Sex = factor(c("m","m","m","m"), levels = c("m","f","indet")),
    variable = c("Fem1l", "Fem1r", "Hum1", "Rad1"),
    value = c(445, 445.4, 312, 240)
  )

  res <- trotter_gleser_1952_an(df)

  expect_s3_class(res, "data.frame")
  expect_equal(rownames(res), "ind1")

  # laterality removed, Fem1 averaged, others excluded
  expect_equal(res["ind1", "bone"], "4. Fem1")

  # n_measures counts both Fem1
  expect_equal(res["ind1", "n_measures"], 2)

  # expected values:
  # if_male: (445.2 * 2.11) + 703.5 = 1642.872
  # if_female: (445.2 * 2.28) + 597.6 = 1612.656
  # if_indet: (1642.872 + 1612.656) / 2 = 1627.764
  tol <- 1
  expect_true(res["ind1", "if_male"] - 1642.872 <= tol)
  expect_true(res["ind1", "if_female"] - 1612.656 <= tol)
  expect_true(res["ind1", "if_indet"] - 1627.764 <= tol)

  # stature should follow the provided sex
  expect_true(res["ind1", "stature"] - 1642.872 <= tol)
})

test_that("trotter_gleser_1952_an returns message when no usable measures are present", {
  df <- data.frame(
    Ind = "ind1",
    Sex = factor(c("m"), levels = c("m","f","indet")),
    variable = "Hum2",
    value = 320
  )

  res <- trotter_gleser_1952_an(df)

  expect_match(
    res,
    "bone measurement"
  )
})
