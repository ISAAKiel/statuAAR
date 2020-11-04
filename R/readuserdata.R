#' Reorganises and checks tabled data for function body.hight
#' 
#' @description 
#' Checks tabled user data and provides a data.frame of standardised measurements 
#' for body statuar calculation with five columns: 
#'     Ind(ividual), Sex, Group, variable, value.
#'  Checks data consitency: 
#'    * uniqueness of individual identifyer,
#'    * accepted values for Sex
#'    * accepted measures names.
#'  Provides a data.frame with summarised statistics for each measure across the sample 
#'     to check for data inconsitancy.
#'   
#'  @param x A simple data.frame containing the measurements per individual.
#'  @param d.form A string defining the data.frame structure.
#'    * d.form=`table` for a data.frame with individuals (rows) and measurements (columns).
#'    * d.form=`list`  for a data.frame with at least two columns: 
#'      `variable`(character, measure name e.g. hum1), `value` (numeric, length (mm)).
#'  @param ind A string defining the column with identifiers for each individual.
#'    If ind = NA a column `Ind` with rownumbers will be added. 
#'  @param sex A string defining the column identifying the sex.
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
#'   \item \bold{Sex}: sex of the individual. Accept values 1 (male), 2 (female), 3 (indet) or `m`(ale), `f`(emale), `indet`.
#'   \item \bold{grp}: a grouping variable (e.g. population).
#'   \item \bold{variable}:  short name of the measure for \bold{value} 
#'   \item \bold{value}: measurement for \bold{measure}.
#' }

# function to create a correlation table for userspecific (own) measure.names
create.measures.list<- function(){
  measures.list<-read.delim("./R/measures.tab", 
                            skip = 1, 
                            quote = "\"",
                            colClasses = c(rep("character",3)))
  measures.list<-measures.list[order(measures.list$short),]
  fix(measures.list)
}

# read user data
prep.user.data <- function (x, d.form='table', ind='Ind', sex='Sex', grp=NA, measures.names='own') {
  td<-x
  
  # basic check of data format
  if (!is.data.frame(td)) {
    stop("Please provide a data.frame with measurements per individual")
  }

  # check the parameters provided by the user
  if (!(d.form %in% c('table', 'list'))) {
    stop("Please indicate the data structure d.form='table' (standard) or d.form='list'")
  }
  # if data is list check if column names variable and value exist
  if (d.form == 'list'){
    if (any(which(colnames(dl)=='variable')==0, 
            which(colnames(dl)=='value')==0)){
      stop("Please provide a column 'variable' with measures.names and 'value' with values")
    }
  }
  # check if user decided on measure.names format 
  if (!(measures.names %in% c('short', 'long', 'own'))) {
    stop("Please indicate the measure.names format 'short', 'long' (standard), 'own'")
  }
  # if user selected to use "own" measure.names: 
  #     1. check if df measures list exists 
  #     2. run function to import standard data from csv into df and open for data input
  if (measures.names == 'own'){
    if(!(exists('measures.list'))){
      tcltk::tk_messageBox(caption = "Data import", 
                          message = paste("Please fill in your corresponding values in the column 'own'.", 
                                          "Or provide a corresponding table: own.table -> measures.list.",
                                          "Or run fix(measures.list) to edit the data frame of the concordance list.",
                                          "Afterwards run the function again.", sep = "\n"), 
                          icon = "info", type = "okcancel")
      create.measures.list()
    }
  }
  
  # check for corresponding measure.names of the column 'own' and data provided by the user will be done after conversion to a list format
  
  # change the column names to 'ind', 'sex' and 'grp' for further processing
  # ind
  if (!(ind %in% names(td))) {
    stop(paste ("Your individual identifier '",ind,"' is not part of the data provided.", sep = ""))
  } else {
    names(td)[which(names(td)==ind)]<-'Ind'
  }
  
  if (all(is.na(td$Ind))){
    warning('No individual identifier provided, each record (row) will be counted as one individual.')
    td<-cbind(td$Ind<-rownames(td),td)
  } else if (any(is.na(td$Ind))) {
      stop('At least one idividual is not labeled. Please check and edit data.')
  }
  
  # check variable sex
  if (!(sex %in% names(td))) {
    stop(paste ("The column name provided for the sex '",sex,"' is not part of the data provided.", sep = ""))
    } else {
      names(td)[which(names(td)==sex)]<-'Sex'
    }

  td$Sex[is.na(td$Sex)] <- 'indet'    
  
  # reduce every value starting with m, f, i, 1, 2, 3 to this character
  # cutting of female, female?, fem., male, indet.  etc.
  td$Sex<-as.character(td$Sex)
  td$Sex<-tolower(td$Sex)
  td$Sex<-gsub("([mfi123])(.*)","\\1", td$Sex)
  # check if there is any other value but ...
  user_sex <- unique(td$Sex)
  wrong_sex <- NULL
  for (i in user_sex){
    if (!(i %in% c('1', '2', '3', 'm', 'f', 'i'))) {
      wrong_sex <- c(wrong_sex,i)
    }
  }
  if (length(wrong_sex)>0) {
    stop(paste("Please provide sex only with 1 alias 'm', 2 alias 'f' or 3 alias 'indet'.",
               paste(wrong_sex, collapse = ", "), sep = "\n "))
  }
  # be shure to have only 1, 2, 3 and make it a factor
  td$Sex[td$Sex=='m'] <- '1'
  td$Sex[td$Sex=='f'] <- '2'
  td$Sex[td$Sex=='i'] <- '3'
  td$Sex<-factor(td$Sex, levels=c('1','2','3'), labels = c('m', 'f', 'indet'))

  # gouping variable 
  if (is.na(grp)){
    td<-cbind(Group=grp,td)
    } else if (!(grp %in% names(td))) {
      stop(paste ("Your grouping variable '",grp,"' is not part of the data provided.", sep = ""))
    } else {
      names(td)[which(names(td)==grp)]<-'Group'
    }
  if (any(!is.na(grp)) & any(is.na(grp))){
    warning('One or more individuals have no grouping value.')
  } 

  # make a data list

  # bring columns 'Ind', 'Sex', 'Group' to the front
  idcols<-c('Ind','Sex', 'Group')
  newcolorder<-c(idcols, names(td)[-which(names(td) %in% idcols)])
  td<-td[newcolorder]

  # for tabled data
  if (d.form=='table'){
    dl<-reshape2::melt(td, id=idcols, na.rm=TRUE)
  } else {
    dl<-td
  }

  # merges the listed measures with the list of measure.names, filters on the columns needed.
  if (measures.names == 'own'){
      result<-merge (dl, measures.list, by.x = 'variable', by.y = 'own')
  } else if (measures.names == 'short'){
      result<-merge (dl, measures.list, by.x = 'variable', by.y = 'short')
  } else if (measures.names == 'long'){
    result<-merge (dl, measures.list, by.x = 'variable', by.y = 'long')
  } else {
    # This should not happen
    Warning('Unexpected value in variable measure.names appeared.')
  }
  result<-result[c('Ind','Sex', 'Group','short','value')]
  names(result)[which(names(result)=='short')]<-'variable'

  # check for duplicated identifiers (individuals)
  if (!is.null(ind)){
    dupl_ind<-NULL
    # any combination of Ind and variable more than 1
    test<-data.frame(cbind(Ind=as.character(result$Ind),
                           variable=gsub("[rl]$", "", result$variable),
                           r.l=gsub(".*([rl])$|.*[^rl]$", "\\1", result$variable)),
                     stringsAsFactors = FALSE)
    # This schould match 2 x left or 2 x right for one measure.
    # But for 1 x left, 1 x right and 1 x NA for one measure it will fail. 
    dupl_ind<-which(plyr::count(test, c('Ind', 'variable', 'r.l'))[,4]>1)
    
    # This should find any measure occuring more than twice per Ind.
    dupl_ind<-c(dupl_ind, which(plyr::count(test, c('Ind', 'variable'))[,3]>2))
    
    if(length(dupl_ind)>0){
      warning(paste("Likely duplicate individuals encountered:",
                    paste(unique(test$Ind[dupl_ind]), collapse= ", "), sep = "\n ")
      )
    }
  }
  # check for inconsistent sex or grouping
  
  # to be done ... plyr::count bla
  
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
  user_measures<-unique(result$variable)
  for (i in 1:length(user_measures)) {
    agg_measures[i,] <- as.list(c(user_measures[i],
           length(subset(result[[5]],result[[4]]==user_measures[i])),
           as.vector(summary(subset(result[[5]],result[[4]]==user_measures[i])))))
  }
  agg_measures[,2:8] <- sapply(agg_measures[,2:8], as.numeric)
  #agg_measures<- cbind(agg_measures, maxDiff2Mean=(agg_measures$MaxM - agg_measures$MinM) * 100/agg_measures$MedianM)
  print (agg_measures)
  statuaar.list<<-result
    }
}
prep.user.data()
