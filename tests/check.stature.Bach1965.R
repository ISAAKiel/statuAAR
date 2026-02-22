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
short<-getFormulaDataframe()[,1]
bb65.estimates <- getStature(short, dl.bach1965)
bb65.estimates.df <- getStatureDataframe(bb65.estimates)

# Merge the original published and the calculated stature.
bb65.check <- merge(Bach1965[5], bb65.estimates.df,
                    by.x='row.names', by.y = 'id')
bb65.check$diff <- bb65.check$stature.x - bb65.check$stature.y

# Calculate the difference.

boxplot(data = bb65.check, diff~formula,
        main = "Stature (Bach 1965) and estimates",
        xlab = "Formula abbreviation (short)",
        ylab = "Orig. stature difference (mm)")
