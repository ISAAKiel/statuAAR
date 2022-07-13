Bach1965 <- read.csv(
  "data-raw/Bach1965.csv",
  sep = ",",
  header = TRUE,
  skip=3,
  stringsAsFactors = FALSE,
  check.names = FALSE
)
usethis::use_data(Bach1965, overwrite = TRUE)

Feldesman_etal_1990 <- read.csv(
  "data-raw/Feldesman_etal_1990.csv",
  sep = ",",
  header = TRUE,
  skip=3,
  stringsAsFactors = FALSE,
  check.names = FALSE
)
usethis::use_data(Feldesman_etal_1990, overwrite = TRUE)

Rollet1888 <- read.csv(
  "data-raw/Rollet1888.csv",
  sep = ",",
  header = TRUE,
  skip=5,
  stringsAsFactors = FALSE,
  check.names = FALSE
)
usethis::use_data(Rollet1888, overwrite = TRUE)

TrotterGleser1952 <- read.csv(
  "data-raw/TrotterGleser1952.csv",
  sep = ",",
  header = TRUE,
  skip=2,
  stringsAsFactors = FALSE,
  check.names = FALSE
)
usethis::use_data(TrotterGleser1952, overwrite = TRUE)

measures.concordance <- read.csv(
  "data-raw/measures.concordance.tab",
  sep = "\t",
  header = TRUE,
  skip=1,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  quote = "\"",
  colClasses = c(rep("character",3))
)
usethis::use_data(measures.concordance, overwrite = TRUE)
