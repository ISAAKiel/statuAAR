# Provide tabled user data as standardised list of measures for sizAAR calculation 

# librarys needed
library(reshape2)

# read user table
td<-read.table('testdata.tab',sep='\t', head=T)
td

# check variable name / column headings
user_measures <- colnames(td[-1])

## column headings: syntax standardisation
user_measures <- gsub("-", "_", user_measures)
user_measures <- gsub(" ", "_", user_measures)
user_measures <- tolower(user_measures)
user_measures <- gsub("(^|[[:space:]])([[:alpha:]])","\\1\\U\\2",user_measures,perl=TRUE)

## column headings: check for non specified labels
  wrong_measures <- NULL

for (i in user_measures){
  if (!(i %in% measures)) {
    wrong_measures <- c(wrong_measures,i)
  }
}
wrong_measures

# transform user data into list
td_<-na.omit(melt(td,id="Individual"))
