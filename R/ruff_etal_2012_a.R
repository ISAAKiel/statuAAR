#' @name ruff_etal_2012_a
#'
#' @title Calculate stature estimation according to: Ruff el al 2012, All (indet).
#'
#' @description
#' Stature estimation (mm) based on the hierarchy of different regression calculations,
#' separated  by sex and region (Ruff et al 2012). The authors provide various formulas,
#' whereby they primarily use the sum of femur and tibia length for the regional groups
#' due to its greater accuracy, while the undifferentiated calculations are based on
#' separate measurements of the femur, humerus and radius. Depending on the measurements
#' provided for each individual, regional assignment may result in a missing stature
#' estimate. In addition, as an alternative to each separately calculated regional group,
#' a calculation can be performed for a mixed data set.
#' The authors do not refer to a hierarchical application of the respective formulas,
#' but the repeated emphasis on %SEE as a quality characteristic suggests this.
#' In addition, averaging the results of both formulas for the respective regional group
#' would inadmissibly include the length of the tibia multiple times.
#' Bone measures used in hierarchical order of percent standard error of estimate (%SEE):
#'
#' If bone measures for left and right are provided the mean value will be used,
#' but for statistic information 2 bones will be counted (n_measures).
#'
#' Returns a data.frame with:
#' \itemize{
#' \item{ ind: individual identifyer (rownames), }
#' \item{ sex: as provided for calculation: m, f, indet.}
#' \item{ stature: estimated on the provided sex and bone measures, }
#' \item{ bone (measure(s)): bones used for calculation, }
#' \item{ female (stature): columns with alternative stature for three sex classes, }
#' \item{ male (stature), }
#' \item{ indet. (stature) and}
#' \item{ n_measures: number of bone measures included:
#'              e.g. 2 Fem2 (left, right) + 1 Tib1}
#' }
#'
#' @param df data.frame of type statuaar_data_table, containing informations on individual, bone and measurement.
#'
#' @return data.frame with calculated stature and related information per individual.
#'
#' @author Christoph Rinne \email{crinne@@ufg.uni-kiel.de}
#'
#' @references
#'   \insertRef{Ruff_Holt_Niskanen_Sladék_Berner_Garofalo_Garvin_Hora_Maijanen_Niinimäki_et al._2012}{statuAAR}
#'
#'   \insertRef{Ruff_2018a}{statuAAR}
#'
#'   \insertRef{Ruff_2018b}{statuAAR}
#'
#' @examples
#' # Read example dataset into a data frame
#'
#'@export

###################################
ruff_etal_2012_a <- function(df){

  df$variable<-gsub("([rl]$)","", df$variable) # laterality not needed
  # check if needed measures are present
  needed <- getFormulaMeasures('r12n')
  if (!any(df$variable %in% needed)){
    return("There is no usable bone measurement / indice available for the chosen formula.")
  }

  # aggregate values for each measure and individual
  df <- aggregate(value ~ Ind + Sex + variable,
                  data = df,
                  FUN = function(x) c(mean = mean(x), n = length(x)))
  df <- do.call(data.frame, df)

  vec_indv <- unique(df$Ind) # extract names and quantity of unique individuals

  # Initialize data frame for later storage of different mean body heights
  val_indv<- as.data.frame(matrix(ncol=7, nrow=length(vec_indv)), row.names=vec_indv)
  colnames(val_indv) <-c("sex", "stature", "bone", "female", "male", "indet", "n_measures")
  val_indv$sex <- factor(val_indv$sex, labels = c("m", "f", "indet"), levels = c(1,2,3))


  # check available values for different variables needed for
  for (i in 1:length(vec_indv)){
    df_bones <- subset(df, subset = Ind==vec_indv[i])

    stature.m <- calc.stature.m(df_bones)
    stature.f <- calc.stature.f(df_bones)
    stature.i <- calc.stature.i(df_bones)

    # vectors for stature estimations, indices and n_measures
    statures <- c(stature.m[[1]], stature.f[[1]], stature.i)
    statures <- round(statures, 0)
    indices <- c(stature.m[[2]], stature.f[[2]], indice.i)
    n_measures <- c(stature.m[[3]], stature.f[[3]], n_measures.i)

    # write values into data frame of results
    val_indv$sex[i] <- unique(df_bones$Sex)
    val_indv$stature[i] <- statures[as.integer(unique(df_bones$Sex))]
    val_indv$bone[i] <- indices[as.integer(unique(df_bones$Sex))]
    val_indv$female[i] <- statures[2]
    val_indv$male[i] <- statures[1]
    val_indv$indet[i] <- statures[3]
    val_indv$n_measures[i] <- n_measures[as.integer(unique(df_bones$Sex))]
  } # next individual

  if (dim(val_indv)[1] == 0) {
    print("There is no usable bone measurement / indice available for the chosen formula")
  }

  #######################################################

calc.stature.m <- function (df_bones){

  # get all optional needed measures
  Fem1 <- df_bones$value.mean[df_bones$variable=="Fem1"]
  Hum1 <- df_bones$value.mean[df_bones$variable=="Hum1"]
  Rad1 <- df_bones$value.mean[df_bones$variable=="Rad1"]

  stature.m<-c()

  stature.m <- (Fem1 * 2.72) + 428.5
  indice <- "Fem1"
  n_measures <- df_bones$value.n[df_bones$variable=="Fem1"]
  if (length(stature.m)==0){
    stature.m <- (Hum1 * 3.83) + 414.2
    indice <- "Hum1"
    n_measures <- df_bones$value.n[df_bones$variable=="Hum1"]
  }
  if (length(stature.m)==0){
    stature.m <- (Rad1 * 4.85) + 474.6
    indice <- "Rad1"
    n_measures <- df_bones$value.n[df_bones$variable=="Rad1"]
  }
  # End of male stature estimation

  return(list("stature"=stature.m, "indice"=indice, "n_measures"=n_measures))

} # End of Function calc.stature.m

##############################################################

calc.stature.f <- function (df_bones){

  # get all optional needed measures
  Fem1 <- df_bones$value.mean[df_bones$variable=="Fem1"]
  Hum1 <- df_bones$value.mean[df_bones$variable=="Hum1"]
  Rad1 <- df_bones$value.mean[df_bones$variable=="Rad1"]

  stature.f<-c()

  stature.f <- (Fem1 * 2.69) + 435.6
  indice <- "Fem1"
  n_measures <- df_bones$value.n[df_bones$variable=="Fem1"]
  if (length(stature.f)==0){
    stature.f <- (Hum1 * 3.38) + 546
    indice <- "Hum1"
    n_measures <- df_bones$value.n[df_bones$variable=="Hum1"]
  }
  if (length(stature.m)==0){
    stature.fm <- (Rad1 * 4.2) + 630.8
    indice <- "Rad1"
    n_measures <- df_bones$value.n[df_bones$variable=="Rad1"]
  }
  # End of female stature estimation

  return(list("stature"=stature.f, "indice"=indice, "n_measures"=n_measures))

} # End of Function calc.stature.f


  ##############################################################

  calc.stature.i <- function (df_bones){

    # get all optional needed measures
    Fem1 <- df_bones$value.mean[df_bones$variable=="Fem1"]
    Hum1 <- df_bones$value.mean[df_bones$variable=="Hum1"]
    Rad1 <- df_bones$value.mean[df_bones$variable=="Rad1"]

    stature.i<-c()

    stature.i <- (Fem1 * 2.77) + 405
    indice <- "Fem1"
    n_measures <- df_bones$value.n[df_bones$variable=="Fem1"]
    if (length(stature.f)==0){
      stature.i <- (Hum1 * 3.72) + 448.6
      indice <- "Hum1"
      n_measures <- df_bones$value.n[df_bones$variable=="Hum1"]
    }
    if (length(stature.f)==0){
      stature.i <- (Rad1 * 4.46) + 569.4
      indice <- "Rad1"
      n_measures <- df_bones$value.n[df_bones$variable=="Rad1"]
    }
    # End of indet. stature estimation

    return(list("stature"=stature.i, "indice"=indice, "n_measures"=n_measures))

  } # End of Function calc.stature.i

  return(val_indv)
}
