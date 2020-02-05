library(reshape2)
# read user table
td<-read.table('testdata.tab',sep='\t', head=T)
td
# check variable name / column headings
# measures <- colnames(td[-1])
user_measures <- colnames(td[-1])

wrong_measures <- NULL

for (i in user_measures){
  if (!(i %in% measures)) {
    wrong_measures <- c(wrong_measures,i)
  }
}
wrong_measures

# transform user data
td_<-na.omit(melt(td,id="Individual"))
