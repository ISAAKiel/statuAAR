test_that("boldsen_1984 aggregates left/right values and computes stature correctly", {
  df <- data.frame(
    Ind = c("ind1", "ind1", "ind1"),
    Sex = c(1, 1, 1),                  # 1 = m, 2 = f, 3 = indet
    variable = c("Fem1l", "Fem1r", "Tib1"),
    value = c(450, 470, 380)
  )

  res <- boldsen_1984(df)

  expect_s3_class(res, "data.frame")
  expect_equal(rownames(res), "ind1")

  # laterality removed, Fem1 averaged, Tib1 kept
  expect_equal(res["ind1", "bone"], "Fem1, Tib1")

  # n_measures counts both Fem1 values plus Tib1
  expect_equal(res["ind1", "n_measures"], 3)

  # expected values:
  # Fem1 mean = (450 + 470) / 2 = 460
  # Male:   mean(460*2.519 + 528.5, 380*2.406 + 823.7) = 1713
  # Female: mean(460*2.528 + 507.6, 380*2.869 + 608.5) = 1685
  # Indet:  mean of male/female estimates = 1699
  expect_equal(res["ind1", "if_male"], 1713)
  expect_equal(res["ind1", "if_female"], 1685)
  expect_equal(res["ind1", "if_indet"], 1699)

  # stature should follow the provided sex
  expect_equal(res["ind1", "stature"], 1713)
})

test_that("boldsen_1984 returns message when no usable measures are present", {
  df <- data.frame(
    Ind = "ind1",
    Sex = 1,
    variable = "Hum1",
    value = 320
  )

  res <- boldsen_1984(df)

  expect_equal(
    res,
    "There is no usable bone measurement / indice available for the chosen formula."
  )
})
