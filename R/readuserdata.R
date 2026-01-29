#' @name readuserdata
#'
#' @title statuAAR data preparation and check functions
#'
#' @description
#' Human stature estimation is based on various measures of different bones. A multitude of
#' formula have been developed and provide a wide range of possible results. The quality of
#' the result depends mainly on the representativity of the original sample of the formula
#' with respect to the currrent study.  Thus a easy and fast forward calculation according
#' to various formula can be used for a camparison of the results.
#'
#' All measures have to be given in millimeters (mm). The measures used are defined by
#' R. Martin (1928). The labels of the measures of the data aquisition may differ from
#' those used by statuAAR. Therefore a concordance of labels should be edited.
#'
#' In addition summarised statistics for each measure across the dataset is provided
#' to check for data inconsitancy befor calculation.
#'
#' \itemize{
#'  \item{\code{\link{create.measures.concordance}}: Creates a data frame with three columns:
#'  \strong{short} names (e.g. Hum1al), \strong{long} names (e.g. Humerus.1a.left) and
#'  \strong{own} to be filled with user defined names.}
#'  \item{\code{\link{measures.statistics}}: Calculates basic descriptive statistics to check
#'  data consitancy.}
#'  \item{\code{\link{prep.statuaar.data}}: Checks the input data: uniqueness of individual
#'  identifyer, accepted values for Sex and accepted measures names.
#'  Provides a data.frame of standardised measurements
#'  for calculation of body stature estimation with four columns:
#'  \strong{Ind}(ividual), \strong{Sex},
#'  \strong{variable} (mearsure name) and \strong{value} (measured value).}
#'  }
#'
#' @rdname readuserdata
#'
#' @param x A simple data.frame containing the measurements per individual.
#' @param d.form A string defining the data.frame structure.
#'  \itemize{
#'    \item{ d.form=`wide` for a data.frame with individuals (rows) and measurements (columns).}
#'    \item{ d.form=`long`  for a data.frame with one measure per individual in one row.
#'          With only two columns: `variable`(character, measure name e.g. hum1),
#'          `value` (numeric, length (mm)), each row represents one individual
#'          with only one measurement and is identified by the row number.}
#'  }
#' @param ind A string defining the column with identifiers for each individual.
#'    If ind = NA a column `Ind` with rownumbers will be added.
#' @param sex A string defining the column identifying the sex.
#'    If sex = NA a column `Sex` with `indet` will be added.
#' @param measures.names A string defining the set of predefined or own measure names used.
#'    For `own` a data.frame `measures.concordance` for correlation (merge) is needed.
#'  \itemize{
#'    \item{ measures=`short`: Bone (3 letters), measure acc. to Martin (1928),
#'      laterality (1 letter) without any separation
#'      (e.g. Hum1, Hum1l, Hum1r, Hum1a, Hum1al, Hum1ar etc.).}
#'    \item{ measures=`long`: Bone measure acc. to Martin (1928), laterality separated by `.`
#'      (e.g. Humerus.1, Humerus.1.left, Humerus.1a.left, etc.).}
#'    \item{ measures=`own`: A data.frame `measures.concordance` with own names to be merged is needed. }
#'  }
#' @param stats Output of aggregating statistics of the measures provided. Default = TRUE.
#' @param dl statuAAR data list as provided by prep.statuaar.data.
#'
#' @return A list with a dataframe for each formula selected.
#'
#' \itemize{
#'   \item \bold{Ind}:  identifyer for each individual.
#'   \item \bold{Sex}: sex of the individual. Accepted values 1 (male), 2 (female), 3 (indet) or `m`(ale), `f`(emale), `indet`.
#'   \item \bold{variable}:  short name of the measure
#'   \item \bold{value}: measured value.
#' }
#' @author Christoph Rinne \email{crinne@@ufg.uni-kiel.de}
#' @author Hendrik Raese \email{h.raese@@ufg.uni-kiel.de}
#'
#' @references
#'  \insertRef{Martin_1928}{statuAAR}
#'
#' @examples
#' # Read example dataset into a data frame
#'

#' # Read example dataset into a data frame
#' x <- TrotterGleser1952
#' # If not yet existent create a list of measure names to be used
#' # measures.concordance <- create.measures.concordance()
#' # Edit the measures.list (not needed for this dataset)
#' # measures.concordance$own[measures.concordance$short=="Fem1"]<-"Fem"
#'
#' # get a dataframe with measures to process
#' dl.trotter.gleser <- prep.statuaar.data(x, d.form = "wide",
#'    ind = "Appendix_row", sex = "Sex", measures.names = "own", stats = FALSE)
#' # See basic statistics to check for errors
#' measures.statistics(dl.trotter.gleser)
#'
#' # For the data from Rollet 1888
#' rollet1888 <- Rollet1888
#' # 1. Create an identifyer due to identical numbering of females and males
#' rollet1888$id<-paste(rollet1888$Sex, rollet1888$Nr, sep="_")
#' # 2. Fill in the mesasures names in the column "own" of the measures.list
#' measures.concordance <- measures.concordance.rollet1888
#' # 3. Read the data
#' dl.rollet1888 <- prep.statuaar.data(rollet1888, d.form = "wide",
#'       ind="id", sex = "Sex", measures.names = "own")
#'
NULL

# function to create a correlation table for userspecific (own) measure.names
#' @rdname readuserdata
#' @export
create.measures.concordance<- function (){
  measures.concordance<-read.delim("./data-raw/measures.concordance.tab",
                            skip = 1,
                            quote = "\"",
                            colClasses = c(rep("character",3)))
  return(measures.concordance[order(measures.concordance$short),])
}

# function to calculate basic statistics for a list of measures
# with $variable for the measure name and $value for the corresponding value
#' @rdname readuserdata
#' @export
measures.statistics <- function (dl) {
  agg_measures<-data.frame(measure=character(),
                           n=integer(),
                           MinM=numeric(),
                           Quart1=numeric(),
                           MedianM=numeric(),
                           MeanM=numeric(),
                           Quart3=numeric(),
                           MaxM=numeric(),
                           stringsAsFactors = FALSE)
  user_measures<-as.character(unique(dl$variable))
  for (i in 1:length(user_measures)) {
    agg_measures[i,] <- as.list(c(user_measures[i],
                                  length(subset(dl[[4]],dl[[3]]==user_measures[i])),
                                  as.vector(round(summary(subset(dl[[4]],dl[[3]]==user_measures[i])),0))))
  }
  agg_measures[,2:8] <- sapply(agg_measures[,2:8], as.numeric)
  return(as.statuaar_statistics(agg_measures))
}

# read user data
#' @rdname readuserdata
#' @import dplyr
#' @export
prep.statuaar.data <- function (x, d.form='wide', ind=NA, sex=NA, measures.names='short', stats = TRUE) {
  td <- x

  # basic check of data format
  if (!is.data.frame(td)) {
    stop("Please provide a data.frame with measurements per individual")
  }

  # check the parameters provided by the user
  if (!(d.form %in% c('wide', 'long'))) {
    stop("Please indicate the data structure d.form='wide' (standard) or d.form='long'")
  }
  # if data is list check if column names variable and value exist
  if (d.form == 'long'){
    if (any(which(colnames(dl)=='variable')==0,
            which(colnames(dl)=='value')==0)){
      stop("Please provide a column 'variable' with measures.names and 'value' with values")
    }
  }
  # check if user decided on measure.names format
  if (!(measures.names %in% c('short', 'long', 'own'))) {
    stop("Please indicate the measure.names format 'short', 'long' (standard), 'own'")
  }
  # get measures concordance if not exists
  if (!exists("measures.concordance")){
    create.measures.concordance()->measures.concordance
  }

  # check for corresponding measure.names of the column 'own' and data provided by the user will be done after conversion to a list format

  # change the column names to 'Ind' and 'Sex' for further processing
  # ind
  if (ind %in% names(td)) {
    names(td)[which(names(td)==ind)]<-'Ind'
  } else if ((is.na(ind)) & (!any(names(td)=='Ind'))) {
    td['Ind']<-rep(NA, nrow(td))
  } else {
    stop(paste ("Your individual identifier '",ind,"' is not part of the data provided.", sep = ""))
  }

  if (all(is.na(td$Ind))){
    warning('No individual identifier provided, each record (row) will be counted as one individual.')
    td$Ind <- rownames(td)
  } else if (any(is.na(td$Ind))) {
    stop('At least one idividual is not labeled. Please check and edit data.')
  }
  if ((d.form=='wide') & (any(duplicated(td$Ind)))) {
    stop(paste("Duplicate individuals (Ind) ecountered:", paste(td$Ind[duplicated(td$Ind)], collapse=", "), sep="\n"))
  }

  # check variable sex
  if (sex %in% names(td)) {
    names(td)[which(names(td)==sex)]<-'Sex'
  } else if (is.na(sex)) {
    td$Sex <- 3
  } else {
      stop(paste ("The column name provided for the sex '",sex,"' is not part of the data provided.", sep = ""))
  }
  td$Sex<-as.character(td$Sex)
  td$Sex[is.na(td$Sex)] <- 'indet'

  # reduce every value starting with m, f, i, 1, 2, 3 to this character
  # cutting of female, female?, fem., male, indet.  etc.
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

  # make a long df

  # bring columns 'Ind' and 'Sex' to the front
  idcols<-c('Ind','Sex')
  newcolorder<-c(idcols, names(td)[-which(names(td) %in% idcols)])
  td<-td[newcolorder]

  # for wide tabled data
  if (d.form=='wide'){
    dl<-reshape2::melt(td, id=idcols, na.rm=TRUE)
  } else {
    dl<-td
  }

  # merges the listed measures with the concordance of measure.names, filters on the columns needed.
  if (measures.names == 'own'){
      result<-merge (dl, measures.concordance, by.x = 'variable', by.y = 'own')
      dl<-result[c('Ind','Sex', 'short','value')]
  } else if (measures.names == 'short'){
      result<-merge (dl, measures.list, by.x = 'variable', by.y = 'short')
      dl<-result[c('Ind','Sex', 'variable','value')]
  } else if (measures.names == 'long'){
      result<-merge (dl, measures.list, by.x = 'variable', by.y = 'long')
      dl<-result[c('Ind','Sex', 'short','value')]
  }
  names(dl)[which(names(dl)=='short')]<-'variable'
  dl$variable <- as.character(dl$variable)
  dl$value <- as.numeric(dl$value)

  # check for duplicated identifiers (individuals)
  dupl_ind<-NULL
  # any combination of Ind and variable occuring more than 1
  test<-data.frame(cbind(Ind=as.character(dl$Ind),
                         # cut off trailing r or l
                         variable=gsub("[rl]$", "", dl$variable),
                         # new variable for left or right
                         r.l=gsub(".*([rl])$|.*[^rl]$", "\\1", dl$variable)),
                         stringsAsFactors = FALSE)
  # This schould match 2 x left or 2 x right for one measure.
  # But for 1 x left, 1 x right and 1 x NA for one measure it will fail.
  dupl_ind<-which(plyr::count(test, c('Ind', 'variable', 'r.l'))[,4]>1)

  # This should find any measure occuring more than twice per Ind.
  dupl_ind<-c(dupl_ind, which(plyr::count(test, c('Ind', 'variable'))[,3]>2))

  if(length(dupl_ind)>0){
    stop(paste("Likely duplicate individuals encountered:",
                  paste(sort(unique(test$Ind[dupl_ind])), collapse= ", "), sep = "\n ")
    )
  }
  # check for inconsistent sex
  if (d.form=='long'){
    test<-data.frame(cbind(Ind=as.character(dl$Ind),
                           Sex=as.character(dl$Sex)),
                           IndSex=as.character(paste(dl$Ind,dl$Sex,sep ="_")),
                           stringsAsFactors = FALSE)
    dupl_sex <- plyr::count(test, c("Ind","Sex"))[1]
    dupl_sex <- dupl_sex$Ind[duplicated(dupl_sex$Ind)]

    if (length(dupl_sex)>0){
      stop(paste("\nLikely inconsistent sex for individual(s) encountered:",
                 paste(dupl_sex, collapse= ", "),)
      )
    }
  }
  if (stats){
    print (measures.statistics(dl))
  }
  return(as.statuaar_data_table(dl))
  }
