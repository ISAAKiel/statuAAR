#' Reorganises and checks tabled data for function body.hight
#' 
#' @description 
#' Checks tabled user data and provides a tibble of standardised measurements 
#' for body hight calculation with three columns: Individual, measurement, value.
#' Provides a tibble with summarised statistics for each measure across the sample to check for data inconsitancy.
#' Provides a tibble with summarised statistics for each measure per individual to check for inconsistancy.
#'   
#'  @param x A simple data.frame containing the measurements per individual.
#'  @param ds A string defining the data.frame structure.
#'    * ds=`table` for a data.frame with individuals (rows) and measurements (columns)
#'    * ds=`list`  for a data.frame with three columns: individual, measurement, value
#'  @param ind A variable / columname with an identifier for each individual.  
#'  @param grp A string defining a optional grouping variable, e.g. sex.
#'    
#'    @return A list of parameters needed for the function \code{body.hight}.
#'    
#' \itemize{
#'   \item \bold{Ind} or \bold{Individual}:  Individual identifyer.
#'   \item \bold{group}: a grouping variable (e.g. sex).
#'   \item \bold{measure}:  abbreviation of the measure (e.g. H1_r) for \bold{value} 
#'   \item \bold{value}: measurement in millimeters (mm) for \bold{measure}
#' }

# librarys needed
#library(reshape2)
library(tidyverse)
library (dplyr)

# for test resons
x<-read.table('testdata.tab',sep='\t', head=T)
ds<-'table'
ind<-'Individual'
grp<- NA
tdl-> td

# read user table
prep.body.hight = function (x, ds = 'table', ind, grp) {
  td<-x
  if (!is.data.frame(td)) {
    stop("Please provide a data.frame with measurements per individual")
  }

  if (!(ds %in% c('table', 'list'))) {
    stop("Please indicate the data structure ds='table' (standard) or ds='list'")
  }

  # change the column names to 'ind' and 'grp' for further processing
  names(td)[which(names(td)==ind)]<-'ind'
  names(td)[which(names(td)==grp)]<-'grp'
  
  # check for duplicated identifiers (individuals)
  if (ds == 'table') {
    dupl_ind<-td$ind[duplicated(td$ind)]
  } else if (ds == 'list') {
   id<-gsub('l', '', td$variable)
   id<-gsub('r', '', id)
   id<-gsub('_', '', id)
   id<-paste(tdl$ind, id, sep = '_')
   dupl_ind<-names(table(id)[table(id)>2])
  }
  
  if (length(dupl_ind)>0) {
    stop(paste("Identifier for Individuals is not unique for:", 
               paste(dupl_ind, collapse=", "), 
               sep=" "))
  }
  
  # transform tabled user data into a list
  if (ds=='table') {
    tdl<-as_tibble(reshape2::melt(td, na.rm=TRUE, id=1))
  } else {
    tdl<-as_tibble(na.omit(td))
  }
  
  # check variable name / column headings
  # variable names: <blank> and "-" have been converted to "."
  tdl[[2]]<-gsub("\\.", "_", tdl[[2]])
  tdl[[2]]<- tolower(tdl[[2]])
  # first character to upper case
  tdl[[2]]<-gsub("(^|[[:space:]])([[:alpha:]])","\\1\\U\\2",tdl[[2]],perl=TRUE)
  
  
  ## column headings: check for non specified labels
  user_measures<-unique(tdl[[2]])
  measures<-scan("./R/measures.txt", what="character", sep = "\n", skip=1)  
  wrong_measures <- NULL
  
  for (i in user_measures){
    if (!(i %in% measures)) {
      wrong_measures <- c(wrong_measures,i)
    }
  }
  
  if (length(wrong_measures)>=1) {
    stop (paste("The following column headings do not match the requirements:",  
                paste(wrong_measures, collapse=", "), sep = "\n"))
   }
  
 # aggegate statistics for data check
  agg_measures<-data.frame(measure=character(), 
                         n=integer(), 
                         MinM=numeric(), 
                         Quart1=numeric(), 
                         MedianM=numeric(), 
                         MeanM=numeric(), 
                         Quart3=numeric(), 
                         MaxM=numeric(),
                         stringsAsFactors = FALSE)
  
  for (i in 1:length(user_measures)) {
    agg_measures[i,] <- as.list(c(user_measures[i],
           length(subset(tdl[[3]],tdl[[2]]==user_measures[i])),
           as.vector(summary(subset(tdl[[3]],tdl[[2]]==user_measures[i])))))
  }
  for (cn in colnames(agg_measures[3:8])) {
     
  }
  agg_measures[,2:8] <- sapply(agg_measures[,2:8], as.numeric)
  agg_measures<-as_tibble(agg_measures)
  agg_measures<- mutate(agg_measures, 
                        maxDiff2Mean = (agg_measures$MaxM - agg_measures$MinM) * 100/agg_measures$MedianM)
  if (any(agg_measures[,9]>1.5)) {
    warning (paste('We calculatet the max. Diff. for each measurement in relation to its mean.',
                  'At least one value differs by more than 1.5%. Please consider to check your data', sep = '\n'))
             print (agg_measures)
  }
}

