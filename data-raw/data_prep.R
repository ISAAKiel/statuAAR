Bach1965 <- read.csv(
  "data-raw/Bach1965.csv",
  sep = ",",
  header = TRUE,
  skip=3,
  stringsAsFactors = FALSE,
  check.names = FALSE
)
usethis::use_data(Bach1965, overwrite = TRUE)

Feldesman1990 <- read.csv(
  "data-raw/Feldesman1990.csv",
  sep = ",",
  header = TRUE,
  skip=3,
  stringsAsFactors = FALSE,
  check.names = FALSE
)
usethis::use_data(Feldesman1990, overwrite = TRUE)

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

# concordance of measure names used by Rollet (1888)
measures.concordance.rollet1888 <- read.csv(
  "data-raw/measures.concordance.rollet1888.tab",
  sep = "\t",
  header = TRUE,
  skip=1,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  quote = "\"",
  colClasses = c(rep("character",3))
)
usethis::use_data(measures.concordance.rollet1888, overwrite = TRUE)

# List of statuaar_formula
bb65 <- list(short = 'bb65', name = 'breitinger_bach_1965', measures = c('Hum2', 'Hum1', 'Rad1b', 'Fem1', 'Tib1b'))
by89 <- list(short = 'by89', name = 'byers_etal_1989', measures = c('Mt11'))
fe90 <- list(short = 'fe90', name = 'feldesman_etal_1990', measures = c('Fem1'))
ff96 <- list(short = 'ff96', name = 'formicola_franceschi_1996', measures = c('Fem2+Tib1', 'Fem1', 'Tib1', 'Hum1', 'Rad1'))
mn09 <- list(short = 'mn09', name = 'maijanen_niskanen_2009', measures = c('Fem2+Tib1', 'Fem1+Tib1', 'Fem2', 'Fem1', 'Tib1', 'Hum1', 'Rad1', 'Fib1', 'Uln1'))
ol78 <- list(short = 'ol78', name = 'olivier_etal_1978', measures = c('Hum2', 'Hum1', 'Rad1b', 'Fem1', 'Tib1b'))
pe99 <- list(short = 'pe99', name = 'pearson_1899', measures = c('Hum1', 'Rad1', 'Fem1', 'Tib1b', 'Tib1a'))
ra08 <- list(short = 'ra08', name = 'raxter_etal_2008', measures = c('Fem1', 'Fem1+Tib1a', 'Fem2', 'Fem2+Tib1a', 'Fem2+Tib1b', 'Hum1', 'Hum1+Rad1', 'Rad1', 'Tib1a', 'Tib1b'))
sj90 <- list(short = 'sj90', name = 'sjovold_1990', measures = c('Hum1', 'Rad1', 'Rad1b', 'Uln1', 'Fem1', 'Fem2', 'Tib1', 'Tib1b', 'Fib1'))
te50 <- list(short = 'te50', name = 'telkkae_1950', measures = c('Hum1', 'Rad2', 'Uln2', 'Fem1', 'Tib1', 'Fib1'))
tg01 <- list(short = 'tg01', name = 'trotter_gleser_1952_an', measures = c('Fem1', 'Tib1b', 'Fib1', 'Uln1', 'Rad1', 'Hum1'))
tg02 <- list(short = 'tg02', name = 'trotter_gleser_1952_aw', measures = c('Fem1', 'Tib1b', 'Fib1', 'Uln1', 'Rad1', 'Hum1'))
ve09 <- list(short = 've09', name = 'vercellotti_etal_2009', measures = c('Fem2+Tib1', 'Fem2', 'Fem1', 'Tib1', 'Hum1+Rad1', 'Hum1', 'Rad1'))
r12n <- list(short = 'r12n', name = 'ruff_etal_2012_n', measures = c('Fem1+Tib1', 'Tib1'))
r12s <- list(short = 'r12s', name = 'ruff_etal_2012_s', measures = c('Fem1+Tib1', 'Tib1'))
r12a <- list(short = 'r12a', name = 'ruff_etal_2012_a', measures = c('Fem1', 'Hum1', 'Rad1'))
statuaar_formula <- list(bb65, by89, fe90, ff96, mn09, ol78, pe99, ra08, sj90, te50,
                         tg01, tg02, ve09, r12a, r12s, r12n)
usethis::use_data(statuaar_formula, overwrite = TRUE)
