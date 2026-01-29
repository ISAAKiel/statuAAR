#' @name byers_etal_1989
#'
#' @title Calculate stature estimation according to: Byers et al 1989.
#'
#' @description
#' Stature estimation (mm) based on a regression calculation of one Metatarsal 1
#' only separated by sex (Byers et al 1989).
#' Bone measures used: Os metatarsale I, apex of the capitulum to the midpoint
#' of the articular surface of the base parallel to the longituninal axis of the bone.
#'
#' If bone measures for left and right are provided the mean value will be used,
#' but for statistic information 2 bones will be counted (n_measures).
#' For archaeological finds, group assignment (Euro-American or Afro-American)
#' is not possible. In addition, the gender-specific estimates for Met1 offer the
#' best accuracy. For this reason, only the following formulas are used:
#' Combined data (indet): St = 634 + 16.8 (Metl),
#' All males: St = 815 + 14.3 (Metl),
#' All females: St = 783 + 13.9 (Metl).
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
#'              e.g. 2 Met1 (left, right)}
#' }
#'
#' @param df data.frame of type statuaar_data_table, containing informations on individual, bone and measurement.
#'
#' @return data.frame with calculated stature and related information per individual.
#'
#' @author Christoph Rinne \email{crinne@@ufg.uni-kiel.de}
#'
#' @references
#'   \insertRef{Byers_Akoshima_Curran_1989}{statuAAR}
#'
#' @examples
#' # Read example dataset into a data frame
#'
#' @export

byers_etal_1989 <- function(df){
  df$variable<-gsub("([rl]$)","", df$variable) # laterality not needed
  # check if needed measures are present
  needed <- getFormulaMeasures('by89')
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
    Met1 <- df_bones$value.mean[df_bones$variable=="Met1"]
    # document number of measures
    n_measures <- df_bones$value.n[df_bones$variable=="Met1"]
    #Calculate
    stature.m <- (Met1 * 14.3) + 815
    stature.f <- (Met1 * 13.9) + 783
    stature.i <- (Met1 * 16.8) + 634

    statures <- round(c(stature.m, stature.f, stature.i), 0)

    # write values into data frame of results
    val_indv$sex[i] <- unique(df_bones$Sex)
    val_indv$stature[i] <- statures[as.integer(unique(df_bones$Sex))]
    val_indv$bone[i] <- "Met1"
    val_indv$female[i] <- statures[2]
    val_indv$male[i] <- statures[1]
    val_indv$indet[i] <- statures[3]
    val_indv$n_measures[i] <- n_measures
  }

  if (dim(val_indv)[1] == 0) {
    print("There is no usable bone measurement / indice available for the chosen formula")
  }

  return(val_indv)
}
