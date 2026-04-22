test_that("telkkae_1950 aggregates left/right values and computes stature correctly", {

  df <- data.frame(
    Ind = c("ind1", "ind1", "ind1", "ind1"),
    Sex = factor(c("m","m","m","m"), levels = c("m","f","indet")),
    variable = c("Fem1l", "Fem1r", "Hum1", "Tib1"),
    value = c(445, 445.4, 312, 380)
  )

  res <- telkkae_1950(df)

  expect_s3_class(res, "data.frame")
  expect_equal(rownames(res), "ind1")

  # laterality removed, Fem1 averaged, others excluded
  expect_equal(res["ind1", "bone"], "Hum1, Fem1, Tib1")

  # n_measures counts both Fem1
  expect_equal(res["ind1", "n_measures"], 4)

  # expected values:
  # for male mean of:
  #  1694 + 2.8 * (312 - 329) = 1646.4
  #  1694 + 2.1 * (445.2 - 455) = 1673.42
  #  1694 + 2.1 * (380 - 362) = 1731.8
  # mean: (1646.4 + 1673.42 + 1731.8) / 3 = 1683.873
  # substract 20 mm: 1683.873 - 20 = 1663.873
  # for female mean of:
  #  1568 + 2.7 * (312 - 307) = 1581.5
  #  1568 + 1.8 * (445.2 - 418) = 1616.96
  #  1568 + 1.9 * (380 - 331) = 1661.1
  # mean: (1581.5 + 1616.96 + 1661.1) / 3 = 1619.853
  # substract 20 mm: 1619.853 - 20 = 1599.853
  # for indet mean: ((male + female) / 2) - 20
  #  ((1683.873 + 1619.853) / 2) - 20 = 1631.863

  tol <- 1
  expect_true(res["ind1", "if_male"] - 1663.873 <= tol)
  expect_true(res["ind1", "if_female"] - 1599.853 <= tol)
  expect_true(res["ind1", "if_indet"] - 1631.863 <= tol)

  # stature should follow the provided sex
  expect_true(res["ind1", "stature"] - 1663.873 <= tol)
})

test_that("telkkae_1950 returns message when no usable measures are present", {
  df <- data.frame(
    Ind = "ind1",
    Sex = factor(c("m"), levels = c("m","f","indet")),
    variable = "Rad1",
    value = 320
  )

  res <- telkkae_1950(df)

  expect_match(
    res,
    "bone measurement"
  )
})
