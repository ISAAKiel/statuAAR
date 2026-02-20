# Read csv data, e.g. Bach1965.csv including stature
Bach1965 <- read.csv(
  "data-raw/Bach1965.csv",
  sep = ",",
  header = TRUE,
  skip=3,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

# Prepare tabled data into a long list (statuaar_data_table)
dl.bach1965 <- prep.statuaar.data(Bach1965, d.form = "wide",
               measures.names = "short", sex = "sex", stats = FALSE)

# Get stature estimations
bb65.estimates <- getStature(c('bb65'), dl.bach1965)

# Merge the original published and the calculated stature.
bb65.check <- merge(Bach1965[5], bb65.estimates[[1]][2], by='row.names')

# Calculate the difference.

hist(bb65.check[,2]-bb65.check[,3],
     main = 'Difference between original and caclucalted stature',
     #xlim = c(-10,10),
     xlab = 'Difference (mm)')
