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
#'  @param ind A variable / columname with an identifier for each sex.
#'  @param grp A string defining a optional grouping variable, e.g. population.
#'    
#'    @return A list of parameters needed for the function \code{body.hight}.
#'    
#' \itemize{
#'   \item \bold{Ind} or \bold{Individual}:  Individual identifyer.
#'   \item \bold{sex}: sex of the individual (f or m), ? will be ignored (m? -> m). 
#'   \item \bold{group}: a grouping variable (e.g. population).
#'   \item \bold{measure}:  abbreviation of the measure (e.g. H1_r) for \bold{value} 
#'   \item \bold{value}: measurement in millimeters (mm) for \bold{measure}
#' }

# librarys needed
#library(reshape2)
library(tidyverse)
library (dplyr)

# for test reasons, will be deleted
x<-read.table('Rollet1888.csv',sep=',', head=T)
x<-read.table('testdata_ind_grp.tab',sep='\t', head=T)
x<-read.table('testdata_input.tab',sep='\t', head=T)
x<-rbind(x,x)
ds<-'table'
ds<-'list'
ind<-'Nr'
ind<-NULL
grp<- NULL
sex<-'Sex'
tdl-> td

# read user table
prep.body.hight = function (x, ds='table', ind=NA, sex= NA, grp=NA) {
  td<-x
  
  # basic check of data format
  if (!is.data.frame(td)) {
    stop("Please provide a data.frame with measurements per individual")
  }

  # check the parameters provided by the user
  if (!(ds %in% c('table', 'list'))) {
    stop("Please indicate the data structure ds='table' (standard) or ds='list'")
  }

  # change the column names to 'ind', 'sex' and 'grp' for further processing
  # ind
  if ((!is.na(ind)) & !(ind %in% names(td))) {
    stop(paste ("Your individual identifier '",ind,"' is not part of the data provided.", sep = ""))
  }
  names(td)[which(names(td)==ind)]<-'ind'
  if (!is.factor(td$ind)){
    td$ind<-factor(td$ind)
  }
  
  # sex
  if ((!is.na(sex)) & !(sex %in% names(td))) {
    stop(paste ("The column name provided for the sex '",sex,"' is not part of the data provided.", sep = ""))
  }
  names(td)[which(names(td)==sex)]<-'sex'
  
  user_sex <- unique(td$sex)
  wrong_sex <- NULL
  for (i in user_sex){
    if (!(i %in% c('1', '2', '3', 'm', 'f', 'indet'))) {
      wrong_sex <- c(wrong_sex,i)
    }
  }
  if (length(wrong_sex)>0) {
    stop("Please provide sex only with 1 alias 'm', 2 alias 'f' or 3 alias 'indet'.")
  }
  
  if (is.numeric(td$sex)){
    td$sex<-factor(td$sex, levels=c('1','2','3'), labels = c('m', 'f', 'indet'))
  }
  
############################## hier geht es weiter
 # gouping variable 
  if ((!is.na(grp)) & !(grp %in% names(td))) {
    stop(paste ("Your grouping variable '",ind,"' is not part of the data provided.", sep = ""))
  }
  #rename the columns for individual identifier and grouping in td
  names(td)[which(names(td)==grp)]<-'grp'
  

  # check for duplicated identifiers (individuals)
  dupl_ind<-NULL
  if (!is.null(ind)) {
    if (ds == 'table') {
      dupl_ind<-td$ind[duplicated(td$ind)]
    } else if (ds == 'list'){
      id<-gsub('_[LlRr]','',td$variable)
     id<-paste(td$ind, id, sep = '_')
     dupl_ind<-names(table(id)[table(id)>2])
    }
  }
  if (length(dupl_ind)>0) {
    stop(paste("Identifier for Individuals is not unique for:", 
               paste(dupl_ind, collapse=", "), 
               sep=" "))
  }
  # if grouping (grp) is NULL all measures a set to group "all"
  if (is.na(grp)) {
    td<-cbind(grp=rep('all', nrow(td)),td)
  } 
  
  # if individual identifyer (ind) is NULL all measures a set to ind "all"
  if (is.na(ind)) {
    td<-cbind(ind=rep('all', nrow(td)),td)
  } 
  
  # transform tabled user data into a list
  if (ds=='table') {
    tdl<-dplyr::as_tibble(reshape2::melt(td, na.rm=TRUE, id=c("ind", "grp")))
  } else {
    tdl<-dplyr::as_tibble(na.omit(td))
  }
  
  # check variable name / column headings
  # variable names: <blank> and "-" have been converted to "."
  tdl[[3]]<-gsub("\\.", "_", tdl[[3]])
  tdl[[3]]<- tolower(tdl[[3]])
  # first character to upper case
  tdl[[3]]<-gsub("(^|[[:space:]])([[:alpha:]])","\\1\\U\\2",tdl[[3]],perl=TRUE)
  
  
  ## column headings: check for non specified labels
  user_measures<-unique(tdl[[3]])
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
           length(subset(tdl[[4]],tdl[[3]]==user_measures[i])),
           as.vector(summary(subset(tdl[[4]],tdl[[3]]==user_measures[i])))))
  }
  agg_measures[,2:8] <- sapply(agg_measures[,2:8], as.numeric)
  agg_measures<-dplyr::as_tibble(agg_measures)
  agg_measures<- dplyr::mutate(agg_measures, 
                        maxDiff2Mean = (agg_measures$MaxM - agg_measures$MinM) * 100/agg_measures$MedianM)
  if (any(agg_measures[,9]>1.5)) {
    warning (paste('We calculatet the max. Diff. for each measurement in relation to its mean.',
                  'At least one value differs by more than 1.5%. Please consider to check your data', sep = '\n'))
             print (agg_measures)
  } else{
    print (agg_measures)
  }
}

