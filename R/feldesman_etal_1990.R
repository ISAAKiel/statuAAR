#' @title Calculate stature estimation according to: Feldesman et al 1990.
#'
#' @name feldesman_etal_1990
#'
#' @description
#' Stature estimation (mm) based on a regression calculation of one bone,
#' not separated  by sex (Feldesman et al 1990).
#' Bone measures used: Fem1
#'
#' If bone measures for left and right are provided the mean value will be used,
#' but for statistic information 2 bones will be counted (n_measures).
#' If sex is indet. the mean of male and female stature estimation is given.
#' Based on the measurement of the Femur (Fem1) of different individuals,
#' the stature for males and females is calculated.
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
#'              e.g. 2 Fem1 (left, right)}
#' }
#'
#' @param df data.frame of type statuaar_data_table, containing informations on individual, bone and measurement.
#'
#' @return data.frame with calculated stature and related information per individual.
#'
#' @author Christoph Rinne \email{crinne@@ufg.uni-kiel.de}
#'
#' @references
#'   \insertRef{Feldesman_Kleckner_Lundy_1990}{statuAAR}
#'
#' @examples
#' # Read example dataset into a data frame
#'
#' @export

feldesman_etal_1990 <- function(df){

  df$variable<-gsub("([rl]$)","", df$variable) # laterality not needed
  # aggregate values for each measure and individual

  # check if needed measures are present
  needed <- getFormulaMeasures('feldesman_etal_1990')
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
  val_indv <- as.data.frame(matrix(ncol=7, nrow=length(vec_indv)), row.names=vec_indv)
  colnames(val_indv) <-c("sex", "stature", "bone", "female", "male", "indet", "n_measures")
  val_indv$sex <- factor(val_indv$sex, labels = c("m", "f", "indet"), levels = c(1,2,3))

  # check available values for different variables needed for
  for (i in 1:length(vec_indv)){
    df_bones <- subset(df, subset=df$Ind == vec_indv[i])
    # Get measure values needed
    Fem1 <- df_bones$mean.value[df_bones$variable=="Fem1"]
    #Calculate
    stature <- (Fem1 * 10) / 26.74

    stature <- round(stature, 0)

    # write values into data frame of results
    val_indv$sex[i] <- unique(df_bones$Sex)
    val_indv$stature[i] <- stature
    val_indv$bone[i] <- "Fem1"
    val_indv$group[i] <- unique(df_bones$Group)
    val_indv$female[i] <- stature
    val_indv$male[i] <- stature
    val_indv$indet[i] <- stature
    val_indv$n_measures[i] <- df_bones$n[df_bones$variable=="Fem1"]
  }

  if (dim(val_indv)[1] == 0) {
    print("There is no usable bone measurement / indice available for the chosen formula")
  }

  return(val_indv)
}
