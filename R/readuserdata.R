# Provide tabled user data as standardised list of measures for sizAAR calculation 

# librarys needed
library(reshape2)

# read user table
td<-read.table('testdata.tab',sep='\t', head=T)
#td

# check variable name / column headings
user_measures <- colnames(td)

## column headings: syntax standardisation
user_measures <- gsub("-", "_", user_measures)
user_measures <- gsub(" ", "_", user_measures)
user_measures <- tolower(user_measures)
#Uppercase first character
user_measures <- gsub("(^|[[:space:]])([[:alpha:]])","\\1\\U\\2",user_measures,perl=TRUE)

#for test purposes only
# user_measures <- c(user_measures, "test")

## column headings: check for non specified labels
measures<-scan("./R/measures.txt", what="character", sep = "\n", skip=1)  
wrong_measures <- NULL

for (i in user_measures){
  if (!(i %in% measures)) {
    wrong_measures <- c(wrong_measures,i)
  }
}

if (length(wrong_measures)>=1) {
  stop (paste("The following column headings do not match the requirements:", wrong_measures, sep="\n"))
}
  
# transform user data into list
td_<-na.omit(melt(td,id="Individual"))
message

# Check values
