#' Reorganises and checks tabled data for function body.hight
#' 
#' @description 
#' Checks tabled user data and provides a data.frame of standardised measurements 
#' for body hight calculation with five columns: 
#'     Ind(ividual), Sex, Group, variable, value.
#'  Checks data consitency: 
#'    * uniqueness of individual identifyer,
#'    * accepted values for Sex
#'    * accepted measures names.
#'  Provides a data.frame with summarised statistics for each measure across the sample 
#'     to check for data inconsitancy.
#'   
#'  @param x A simple data.frame containing the measurements per individual.
#'  @param ds A string defining the data.frame structure.
#'    * ds=`table` for a data.frame with individuals (rows) and measurements (columns).
#'    * ds=`list`  for a data.frame with at least two columns: 
#'      `variable`(character), `value` (numeric).
#'  @param ind A string defining the column with identifiers for each individual.
#'    If ind = NA a column `Ind` with `NA` will be added. 
#'  @param sex A string defining the column identifying the sex.
#'    Accepts 1 (male), 2 (female), 3 (indet) or `m`(ale), `f`(emale), `indet`.
#'    If sex = NA a column `Sex` with `indet` will be added.  
#'  @param grp A string defining a optional grouping variable, e.g. population.
#'    If grp = NA a column `Group` with `NA` will be added. 
#'  @param measures.names A string defining the set of predefined or own measure names used.
#'    For `own` a data.frame `measures.list` for correlation (merge) is needed. 
#'    This will be created when missing and opend for editing.
#'    * measures=`short`: Bone (3 letters), measure acc. Martin 1928, 
#'      laterality (1 letter) without any separation
#'      (e.g. Hum1, Hum1l, Hum1r, Hum1a, Hum1al, Hum1ar etc.).
#'    * measures=`long`: Bone, measure acc. Martin 1928, laterality separated by `.`
#'      (e.g. Humerus.1, Humerus.1.left, Humerus.1a.left, etc.).
#'    * measures=`own`: A data.frame `measures.list` with own names to be merged is needed. 
#'          
#'  @return A list of parameters needed for the function \code{body.hight}.
#'    
#' \itemize{
#'   \item \bold{Ind} or \bold{Individual}:  individual identifyer.
#'   \item \bold{Sex}: sex of the individual. 
#'   \item \bold{Group}: a grouping variable (e.g. population).
#'   \item \bold{variable}:  short name of the measure for \bold{value} 
#'   \item \bold{value}: measurement for \bold{measure}.
#' }

#librarys needed
#library(reshape2)
#library(tidyverse)
#library (dplyr)

# for test reasons, will be deleted
x<-read.table('Rollet1888.csv',sep=',', head=T)
x<-read.table('TrotterGleser1952.csv',sep=',', head=T)
ds<-'table'
ind<-'Appendix_row'
grp<-"Race"
sex<-'Sex'
measures.names<-'own'

ds<-'list'
ind<-NULL
grp<- NULL
tdl-> td

# read user table
prep.body.hight = function (x, ds='table', ind=NA, sex=NA, grp=NA, measures.names='long') {
  td<-x
  
  # basic check of data format
  if (!is.data.frame(td)) {
    stop("Please provide a data.frame with measurements per individual")
  }

  # check the parameters provided by the user
  if (!(ds %in% c('table', 'list'))) {
    stop("Please indicate the data structure ds='table' (standard) or ds='list'")
  }
  if (ds == 'list'){
    if (any(which(colnames(dl)=='variable')==0, 
            which(colnames(dl)=='value')==0)){
      stop("Please provide a column 'variable' with measures and 'value' with values")
    }
  }
  if (!(measures.names %in% c('short', 'long', 'own'))) {
    stop("Please indicate the measure.names format 'short', 'long' (standard), 'own'")
  }
  if (measures.names == 'own'){
    if(!(exists('measures.list'))){
      measures.list<-read.delim("./lokal/measures.tab", 
                 skip = 1, 
                 quote = "\"",
                 colClasses = c(rep("character",3)))
      fix(measures.list)
    }
  }
  
  # change the column names to 'ind', 'sex' and 'grp' for further processing
  # ind
  if ((!is.na(ind)) & !(ind %in% names(td))) {
    stop(paste ("Your individual identifier '",ind,"' is not part of the data provided.", sep = ""))
  } 
  if (is.null(ind)) {
    td<-cbind(Ind=rep(NA, nrow(td)),td)
  } else {
    names(td)[which(names(td)==ind)]<-'Ind'
    if (!is.factor(td$Ind)){
      td$Ind<-factor(td$Ind)
    }
  }
  
  # sex
  if ((!is.na(sex)) & !(sex %in% names(td))) {
    stop(paste ("The column name provided for the sex '",sex,"' is not part of the data provided.", sep = ""))
  } 
  if (is.na(sex)) {
    td<-cbind(Sex=rep('indet', nrow(td)),td)
  } else {
    names(td)[which(names(td)==sex)]<-'Sex'
  }
  td$Sex<-gsub("[\\?\\.]", "", td$Sex)
  td$Sex<-tolower(td$Sex)
  user_sex <- unique(td$Sex)
  wrong_sex <- NULL
  for (i in user_sex){
    if (!(i %in% c('1', '2', '3', 'm', 'f', 'indet'))) {
      wrong_sex <- c(wrong_sex,i)
    }
  }
  if (length(wrong_sex)>0) {
    stop(paste("Please provide sex only with 1 alias 'm', 2 alias 'f' or 3 alias 'indet'.",
               paste(wrong_sex, collapse = ", "), sep = "\n "))
  }
  if (is.na(as.numeric(td$Sex[1]))){
    td$Sex<-factor(td$Sex, levels= c('m', 'f', 'indet'))
  } else { 
    td$Sex<-factor(td$Sex, levels=c('1','2','3'), labels = c('m', 'f', 'indet'))
  }
  
  # gouping variable 
  if ((!is.na(grp)) & !(grp %in% names(td))) {
    stop(paste ("Your grouping variable '",grp,"' is not part of the data provided.", sep = ""))
  }
  if (is.null(grp)){
    td<-cbind(Group=rep(NA, nrow(td)),td)
  } else {
    names(td)[which(names(td)==grp)]<-'Group'
  }

  # make a data list
  datainfos<-"Information on data provided:"
  dataproblems<-"Problems with the data provided:"

  # bring columns 'Ind', 'Sex', 'Group' to the front
  idcols<-c('Ind','Sex', 'Group')
  newcolorder<-c(idcols, names(td)[-which(names(td) %in% idcols)])
  td<-td[newcolorder]

  # for tabled data
  if (ds=='table'){
    dl<-reshape2::melt(td, id=idcols, na.rm=TRUE)
  } else {
    dl<-td
  }

  result<-merge (dl, measures.list, by.x = 'variable', by.y = 'own')
  result<-result[c('Ind','Sex', 'Group','short','value')]
  names(result)[which(names(result)=='short')]<-'variable'

  # check for duplicated identifiers (individuals)
  if (!is.null(ind)){
    dupl_ind<-NULL
    # any combination of Ind and variable more than 1
    test<-data.frame(cbind(Ind=result$Ind,
                           variable=gsub("[rl]$", "", result$variable),
                           r.l=gsub(".*([rl])$", "\\1", result$variable)))
    
    any(plyr::count(test, c('Ind', 'variable', 'r.l'))[,3]>1)
  ############## hier gehts weiter  
  }
  
  if (length(dupl_ind)>0) {
    stop(paste("Identifier for Individuals is not unique for:", 
               paste(dupl_ind, collapse=", "), 
               sep=" "))
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

