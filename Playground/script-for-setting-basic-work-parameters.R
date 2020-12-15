x<-read.csv("Rollet1888.csv", header = TRUE, skip=5)
d.form='table'
ind='Ind'
ind=NA
sex="Sex"
grp=NA
measures.names='own'

td$Ind<-paste(td$Ind,td$Sex,sep = "_")

td<- x

measures.list<-read.csv("measures.list.rollet1888.csv",header = T, sep = ",")

measures.list <- read.delim("./R/measures.tab", 
                            skip = 1, 
                            quote = "\"",
                            colClasses = c(rep("character",3)))
dl<-read.csv("statuaar.list.csv", header = T, sep = ',')


idcols<-c('Ind','Sex', 'Group')
newcolorder<-c(idcols, names(td)[-which(names(td) %in% idcols)])
td<-td[newcolorder]

dl<-reshape2::melt(td, id=c("Nr","Sex"), na.rm=TRUE)

result<-merge (dl, measures.list, by.x = 'variable', by.y = 'long')
