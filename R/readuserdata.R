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
#'    
#'    @return A list of parameters needed for the function \code{body.hight}.
#'    
#' \itemize{
#'   \item \bold{Ind} or \bold{Individual}:  Individual identifyer
#'   \item \bold{measure}:  abbreviation of the measure (e.g. H1_r) for \bold{value} 
#'   \item \bold{value}: measurement in millimeters (mm) for \bold{measure}
#' }

# librarys needed
library(reshape2)
library(tidyverse)

# for test resons
x<-read.table('testdata.tab',sep='\t', head=T)
ds<-'table'

# read user table
prep.body.hight = function (x, ds = 'table') {
  td<-x
  if (!is.data.frame(td)) {
    stop("Please provide a data.frame with measurements per individual")
  }

  if (!(ds %in% c('table', 'list'))) {
    stop("Please indicate the data structure ds='table' (standard) or ds='list'")
  }

  # transform tabled user data into a list
  if (ds=='table') {
    tdl<-as_tibble(melt(td, na.rm=TRUE, id=1))
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
    stop (paste("The following column headings do not match the requirements:", wrong_measures, sep="\n"))
  }
  
 # aggegate statistics for data check
# in work!!!, result should be a table with: n, Min., 1st Qu., Median, Mean, 3rd Qu., Max. 
  agg_measures<-tibble(measure=character(), n=integer(), MinM=numeric(), Quart1=numeric(), MedianM=numeric(), MeanM=numeric(), Quart3=numeric(), MaxM=numeric())
  for (var in user_measures) {
   add_row(agg_measures,
    measure = var,
    n = length(subset(tdl[[3]],tdl[[2]]==var)))#,
    #MinM:MaxM = as.vector(summary(subset(tdl[[3]],tdl[[2]]==var))))
  }
}

