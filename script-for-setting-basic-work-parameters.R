x<-read.csv("Rollet1888.csv", header = TRUE, skip=5)
d.form='table'
ind='Nr'
ind=NA
sex="Sex"
grp=NA
measures.names='own'

td<- x
measures.list <- read.delim("./R/measures.tab", 
                            skip = 1, 
                            quote = "\"",
                            colClasses = c(rep("character",3)))

idcols<-c('Ind','Sex', 'Group')
newcolorder<-c(idcols, names(td)[-which(names(td) %in% idcols)])
td<-td[newcolorder]

dl<-reshape2::melt(td, id=c("Nr","Sex"), na.rm=TRUE)

result<-merge (dl, measures.list, by.x = 'variable', by.y = 'long')
