#' @name byers_etal_1989
#'
#' @title Calculate stature estimation according to: Byers et al 1989.
#'
#' @description
#' Stature estimation (mm) based on a regression calculation of one bone,
#' not separated  by sex (Byers et al 1989).
#' Bone measures used: Os metatarsale I, apex of the capitulum to the midpoint
#' of the articular surface of the base parallel to the longituninal axis of the bone.
#'
#' If bone measures for left and right are provided the mean value will be used,
#' but for statistic information 2 bones will be counted (n_measures).
#' The  correlation coefficient does not increase with sex differentiation and thus
#' will be ignored. Based on the length of the Os metatarsale I of different individuals,
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
#'              e.g. 2 Fem2 (left, right) + 1 Tib1}
#' }
#'
#' @param df data.frame containing informations on individual, bone and measurement.
#'
#' @return data.frame with calculated stature and related information per individual.
#'
#' @author Christoph Rinne \email{crinne@@ufg.uni-kiel.de}
#'
#' @examples
#'
#' @export

library(dplyr)

byers_etal_1989 <- function(df){

  df$variable<-gsub("([rl]$)","", df$variable) # laterality not needed
  # aggregate values for each measure and individual
  options(dplyr.summarise.inform = FALSE)
  df %>%
    group_by(Ind, Sex, Group, variable) %>%
    summarise(mean.value = mean(value), n = n()) %>%
    as.data.frame -> df

  vec_indv <- unique(df$Ind) # extract names and quantity of unique individuals

  # Initialize data frame for later storage of different mean body heights
  val_indv <- as.data.frame(matrix(ncol=8, nrow=length(vec_indv)), row.names=vec_indv)
  colnames(val_indv) <-c("sex", "group", "stature", "bone", "female", "male", "indet", "n_measures")
  val_indv$sex <- factor(val_indv$sex, labels = c("m", "f", "indet"), levels = c(1,2,3))

  # check available values for different variables needed for
  for (i in 1:length(vec_indv)){
    df_bones <- subset(df, subset=df$Ind == vec_indv[i])
    # Get measure values needed
    Mt11 <- df_bones$mean.value[df_bones$variable=="Mt11"]
    #Calculate
    stature <- (Mt11 * 16.8) + 634

    stature <- round(stature, 0)

    # write values into data frame of results
    val_indv$sex[i] <- unique(df_bones$Sex)
    val_indv$stature[i] <- stature
    val_indv$bone[i] <- "Mt11"
    val_indv$group[i] <- unique(df_bones$Group)
    val_indv$female[i] <- stature
    val_indv$male[i] <- stature
    val_indv$indet[i] <- stature
    val_indv$n_measures[i] <- df_bones$n[df_bones$variable=="Mt11"]
  }

  if (dim(val_indv)[1] == 0) {
    print("There is no usable bone measurement / indice available for the chosen formula")
  }

  return(val_indv)
}
