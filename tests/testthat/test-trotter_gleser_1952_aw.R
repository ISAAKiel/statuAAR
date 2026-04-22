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
  # if_male: (445.2 * 2.38) + 614.1 = 1673.676
  # if_female: (445.2 * 2.47) + 541.0 = 1640.644
  # if_indet: (1673.676 + 1640.644) / 2 = 1657.16
  tol <- 1
  expect_true(res["ind1", "if_male"] - 1673.676 <= tol)
  expect_true(res["ind1", "if_female"] - 1640.644 <= tol)
  expect_true(res["ind1", "if_indet"] - 1657.16 <= tol)

  # stature should follow the provided sex
  expect_true(res["ind1", "stature"] - 1673.676 <= tol)
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
