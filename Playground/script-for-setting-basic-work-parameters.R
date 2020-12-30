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

# Tranformation of Rollet 1888
load("./data/rollet1888.rda")
# create measures.list
measures.list <- create.measures.list()
# set Rollets measure names in column "own"
measures.list$own[measures.list$short=="Fem1r"]<-"Femur.right"
measures.list$own[measures.list$short=="Fem1l"]<-"Femur.left"
measures.list$own[measures.list$short=="Tib1r"]<-"Tibia.right"
measures.list$own[measures.list$short=="Tib1l"]<-"Tibia.left"
measures.list$own[measures.list$short=="Fib1r"]<-"Fibula.right"
measures.list$own[measures.list$short=="Fib1l"]<-"Fibula.left"
measures.list$own[measures.list$short=="Hum1r"]<-"Humerus.right"
measures.list$own[measures.list$short=="Hum1l"]<-"Humerus.left"
measures.list$own[measures.list$short=="Rad1r"]<-"Radius.right"
measures.list$own[measures.list$short=="Rad1l"]<-"Radius.left"
measures.list$own[measures.list$short=="Uln1r"]<-"Ulna.right"
measures.list$own[measures.list$short=="Uln1l"]<-"Ulna.left"

# create identifyer
rollet1888$ind<-paste(rollet1888$Nr, rollet1888$Sex, sep = "_")

# prep.statuaar.data
rollet1888.list<-prep.statuaar.data(rollet1888, ind = "ind", sex = "Sex", measures.names = "own")

df<-rollet1888.list[[2]]


# Read example dataset into a data frame
x <- read.csv("./data-raw/TrotterGleser1952.csv", header=TRUE, skip=2)
# If not yet existent create a list of measure names to be used
measures.list <- create.measures.list()
# Edit the measures.list (not needed for this dataset)
fix(measures.list)
# get a list with [[1]] basic statistics and [[2]] a long dataframe with measures
my.list <- prep.statuaar.data(x, d.form = "table", ind = "Appendix_row", sex = "Sex", grp = "Race")
# With a simple long list of measures call basic statistics to check for errors
measures.statistics(my.list[[2]])
