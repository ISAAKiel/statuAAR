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

# concordance of measure names
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

# List of formula
bb65 <- list(name = 'breitinger_bach_1965', measures = c('Hum2', 'Hum1', 'Rad1b', 'Fem1', 'Tib1b'))
by89 <- list(name = 'byers_etal_1989', measures = c('Mt11'))
fe90 <- list(name = 'feldesman_etal_1990', measures = c('Fem1'))
ff96 <- list(name = 'formicola_franceschi_1996', measures = c('Fem2+Tib1', 'Fem1', 'Tib1', 'Hum1', 'Rad1'))
mn09 <- list(name = 'maijanen_niskanen_2009', measures = c('Fem2+Tib1', 'Fem1+Tib1', 'Fem2', 'Fem1', 'Tib1', 'Hum1', 'Rad1', 'Fib1', 'Uln1'))
ol78 <- list(name = 'olivier_etal_1978', measures = c('Hum2', 'Hum1', 'Rad1b', 'Fem1', 'Tib1b'))
pe99 <- list(name = 'pearson_1899', measures = c('Hum1', 'Rad1', 'Fem1', 'Tib1b', 'Tib1a'))
ra08 <- list(name = 'raxter_etal_2008', measures = c('Fem1', 'Fem1+Tib1a', 'Fem2', 'Fem2+Tib1a', 'Fem2+Tib1b', 'Hum1', 'Hum1+Rad1', 'Rad1', 'Tib1a', 'Tib1b'))
sj90 <- list(name = 'sjovold_1990', measures = c('Hum1', 'Rad1', 'Rad1b', 'Uln1', 'Fem1', 'Fem2', 'Tib1', 'Tib1b', 'Fib1'))
te50 <- list(name = 'telkkae_1950', measures = c('Hum1', 'Rad2', 'Uln2', 'Fem1', 'Tib1', 'Fib1'))
tg01 <- list(name = 'trotter_gleser_1952_an', measures = c('Fem1', 'Tib1b', 'Fib1', 'Uln1', 'Rad1', 'Hum1'))
tg02 <- list(name = 'trotter_gleser_1952_aw', measures = c('Fem1', 'Tib1b', 'Fib1', 'Uln1', 'Rad1', 'Hum1'))
ve09 <- list(name = 'vercellotti_etal_2009', measures = c('Fem2+Tib1', 'Fem2', 'Fem1', 'Tib1', 'Hum1+Rad1', 'Hum1', 'Rad1'))
formula <- list(by89, fe90, ff96, mn09, ol78, pe99, ra08, sj90, te50, tg01, tg02, ve09)
usethis::use_data(formula, overwrite = TRUE)
