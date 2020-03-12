#' Reorganises and checks tabled data for function body.hight
#' 
#' @description 
#' Checks tabled user data and provides a data.frame of standardised measurements 
#' for body hight calculation with five columns: 
#'     Ind(ividual), Sex, Group, variable, value.
#'  Checks data consitency: 
#'    * uniqueness of individual identifyer,
#'    * accepted values for Sex
#'    * accepted measure names.
#'  Provides a data.frame with summarised statistics for each measure across the sample 
#'     to check for data inconsitancy.
#'   
#'  @param x A simple data.frame containing the measurements per individual.
#'  @param ds A string defining the data.frame structure.
#'    * ds=`table` for a data.frame with individuals (rows) and measurements (columns)
#'    * ds=`list`  for a data.frame with at least two columns: variable, value
#'  @param ind A string defining the column with identifiers for each individual.
#'    If ind = NA a column `Ind` with `all` will be added. 
#'  @param sex A string defining the column identifying the sex.
#'    Accepts 1 (male), 2 (female), 3 (indet) or m(ale), f(emale), indet.
#'    If sex = NA a column `Sex` with `indet` will be added.  
#'  @param grp A string defining a optional grouping variable, e.g. population.
#'    If grp = NA a column `Group` with `all` will be added. 
#'  @param measure.names A string defining the set of predefined measure names used.
#'    These are the columne names of tabled data or measure names in the column `variable`.
#'    * measures=`short`: Bone (3 letters), measure acc. Martin 1928, 
#'      laterality (1 letter) without any separation
#'      (e.g. Hum1, Hum1l, Hum1r, Hum1a, Hum1al, Hum1ar etc.).
#'    * measures=`long`: Bone, measure acc. Martin 1928, laterality separated by `.`
#'      (e.g. Humerus.1, Humerus.1.left, Humerus.1a.left, etc.).
#'    Before checking these labes following changes will be performed:
#'    * All measures will be set in 'Propper Case'.
#'    * Short forms H1, H1l, F1 etc. will be renamed to Hum1, Hum1l, Fem1 etc  
#'    * Humerus, Humerus.left etc. will be converted to Humerus.1, Humerus.1.left
#'      making Martins measure 1 (Caput humeri - Trochlea) explicit.
#'    * Undesired punctuation (".", "_", "-", " ") will be deleted.
#'    * Any combination of ri..., le..., li... or re... to the end of the name,
#'      will be replaced by the first letter r or l respectivly. 
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

# librarys needed
#library(reshape2)
#library(tidyverse)
library (dplyr)

# for test reasons, will be deleted
x<-read.table('Rollet1888.csv',sep=',', head=T)
x<-read.table('TrotterGleserTbl13.csv',sep=',', head=T)
ds<-'table'
ds<-'list'
ind<-'Appendix_row'
ind<-NULL
grp<-"Race"
grp<- NULL
sex<-'Sex'
tdl-> td
measure.names<-'long'

# read user table
prep.body.hight = function (x, ds='table', ind=NA, sex=NA, grp=NA, measure.names='long') {
  td<-x
  
  # basic check of data format
  if (!is.data.frame(td)) {
    stop("Please provide a data.frame with measurements per individual")
  }

  # check the parameters provided by the user
  if (!(ds %in% c('table', 'list'))) {
    stop("Please indicate the data structure ds='table' (standard) or ds='list'")
  }
  if (!(measure.names %in% c('short', 'long'))) {
    stop("Please indicate the measure.names format 'short' or 'long' (standard)")
  }
  
  # change the column names to 'ind', 'sex' and 'grp' for further processing
  # ind
  if ((!is.na(ind)) & !(ind %in% names(td))) {
    stop(paste ("Your individual identifier '",ind,"' is not part of the data provided.", sep = ""))
  } 
  if (is.null(ind)) {
    td<-cbind(Ind=rep('all', nrow(td)),td)
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
    stop("Please provide sex only with 1 alias 'm', 2 alias 'f' or 3 alias 'indet'.")
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
    td<-cbind(Group=rep('all', nrow(td)),td)
  } else {
    names(td)[which(names(td)==grp)]<-'Group'
  }

# check variable names

  datainfos<-"Information on data provided:"
  dataproblems<-"Problems with the data provided:"
  
  
  if (ds='table'){
    # for tabled data
    orig_names<-names(td)
    new_names<-orig_names
    new_names<- tolower(new_names)
    # first character to upper case
    new_names<-gsub("(^|[[:space:]])([[:alpha:]])","\\1\\U\\2",new_names,perl=TRUE)
    if(measure.names=='short'){
      # append short names to three letters
      new_names<-gsub("H([12])","Hum\\1", new_names)
      new_names<-gsub("R([12])","Rad\\1", new_names)
      new_names<-gsub("U([12])","Uln\\1", new_names)
      new_names<-gsub("F([12])","Fem\\1", new_names)
      new_names<-gsub("T([12])","Tib\\1", new_names)
      #delete obsolet signs
      new_names<-gsub("[\\._ -]", "", new_names)
      #replace any variation of re(echts), ri(ght), le(ft), li(nks) by the first matching letter
      new_names<-gsub("([rl])[ei].*$","\\1", new_names)
    } else {
      # add measure "1" if not explicit in the name
      new_names<-gsub("Humerus$|Humerus(\\.[rl])","Humerus.1\\1", new_names)
      new_names<-gsub("Radius$|Radius(\\.[rl])","Radius.1\\1", new_names)
      new_names<-gsub("Ulna$|Ulna(\\.[rl])","Ulna.1\\1", new_names)
      new_names<-gsub("Femur$|Femur(\\.[rl])","Femur.1\\1", new_names)
      new_names<-gsub("Tibia$|Tibia(\\.[rl])","Tibia.1\\1", new_names)
      new_names<-gsub("Fibula$|Fibula(\\.[rl])","Fibula.1\\1", new_names)
      #delete obsolet signs
      new_names<-gsub("[_ -]?", "", new_names)
    }

    names(td)<-new_names
    if (!all(new_names == orig_names)){
      datainfos<-paste(datainfos, paste("Columns renamed:", 
                       paste(new_names, collapse=", "), sep = "\n "), 
                       sep = "\n ")
    }
    
    # bring columns 'Ind', 'Sex', 'Group' to the front
    idcols<-c('Ind','Sex', 'Group')
    newcolorder<-c(idcols, names(td)[-which(names(td) %in% idcols)])
    td<-td[newcolorder]
    
    # check if  measure_names are known
    if (measure.names=='long') {
      accepted_names<-scan("./R/measures.txt", what=list("",""), skip=2)[[2]]
    } else {
      accepted_names<-scan("./R/measures.txt", what=list("",""), skip=2)[[1]]
    }
    
    all_names<-names(td)[4:ncol(td)]
    wrong_names <- NULL
    measure_names <- NULL
    for (i in all_names){
      if (i %in% accepted_names) {
        measure_names <- c(measure_names, i)
      } else {
        wrong_names <- c(wrong_names,i)
      }
    }
    if (length(wrong_names > 0)){
      datainfos<-paste(datainfos, paste("Unkown columns will be skiped:", 
                                        paste(wrong_names, collapse=", "), sep = "\n "),
                       sep = "\n ")
    }
  dl<-reshape2::melt(td, id=idcols, na.rm=TRUE, measure=measure_names)
  } else {
    # data provided as list: check measure names and measures
    
    # bring columns 'Ind', 'Sex', 'Group' to the front
    idcols<-c('Ind','Sex', 'Group')
    newcolorder<-c(idcols, names(td)[-which(names(td) %in% idcols)])
    td<-td[newcolorder]
    all_names<-NULL
    column_numeric<-NULL

    # look for further variables
    for (i in names(td[4:ncol(td)])){
      all_names<-c(all_names, i)
    }
    for (i in td[4:ncol(td)]){
      column_numeric <- c(column_numeric, is.numeric(i))
    }
    # if there is no column 'variable'
    if (which('variable' %in% all_names) != 1){
      if (length(all_names) - (sum(column_numeric))>1){
        datainfos<-paste(datainfos, "There is more than on non integer column, we select the first as variable.",
                         sep = "\n ")
      }
      names(td[3 + min(which(column_numeric == FALSE))])<-'variable'
    }
    # if there is no column 'value'
    if (which('value' %in% all_names) != 1){
      if (sum(column_numeric)>1){
        datainfos<-paste(datainfos, "There is more than on integer column, we select the first as value.",
                         sep = "\n ")
      }
      names(td[3 + min(which(column_numeric == TRUE))])<-'variable'
    }
  }
  
  ############### hier geht es weiter ##########################
  # now check the measure names in column variable
  if (nchar(datainfos)>30){
    warning(datainfos)
  }
  
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

