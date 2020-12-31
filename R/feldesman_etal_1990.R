#' Calculate body height indices and mean body height based on Feldesman et al 1990.
#' 
#' Based on the available measurements of different bones of different individuals, 
#' the body height indices for males and females are calculated.
#'
#' 
#' @title feldesman_etal_1990
#'
#' @description 
#' Based on the measurement of the Femur (Fem1) of different individuals, 
#' the stature for males and females are calculated.
#'
#' Returns a data.frame with: 
#'     * ind: individual identifyer (rownames), 
#'     * sex: 
#'     * stature: estimated on the provided sex and bone measures, 
#'     * bone (measure(s)): bones used for calculation, 
#'     * female (stature): columns with alternative stature for three sex classes, 
#'     * male (stature), 
#'     * indet. (stature) and
#'     * n_measures: number of bone measures included, e.g. (left + right)/2
#' 
#' @param df data.frame, containing informations on individual, bone and measurement
#'  
#' @return data.frame, containing one data.frame with all calculated indices for every individual
#'           
#' @author Christoph Rinne <\email{crinne@@ufg.uni-kiel.de}>
#' 
#' @examples
#' 
#' @export

library(dplyr)

feldesman_etal_1990 <- function(df){
  
  df$variable<-gsub("([rl]$)","", df$variable) # laterality not needed
  # aggregate values for each measure and individual
  df %>%  
    group_by(Ind, Sex, Group, variable) %>% 
    summarise(mean.value = mean(value), n = n()) %>%
    as.data.frame -> df
  
  vec_indv <- unique(df$Ind) # extract names and quantity of unique individuals
  
  # Initialize data frame for later storage of different mean body heights
  val_indv <- as.data.frame(matrix(ncol=7, nrow=length(vec_indv)), row.names=vec_indv)
  colnames(val_indv) <-c("sex", "stature", "bone", "female", "male", "indet", "n_measures")
  val_indv$sex <- factor(val_indv$sex, labels = c("m", "f", "indet"), levels = c(1,2,3))
  
  # check available values for different variables needed for 
  for (i in 1:length(vec_indv)){
    df_knochen <- subset(df, subset=df$Ind == vec_indv[i])
    # Get measure values needed
    Fem1 <- df_knochen$mean.value[df_knochen$variable=="Fem1"]
    #Calculate
    stature <- (Fem1 * 10) / 26.74

    stature <- round(stature, 0)

    # write values into data frame of results
    val_indv$sex[i] <- unique(df_knochen$Sex)
    val_indv$stature[i] <- stature
    val_indv$bone <- "Fem1"
    val_indv$female[i] <- stature
    val_indv$male[i] <- stature
    val_indv$indet[i] <- stature
    val_indv$n_measures[i] <- df_knochen$n[df_knochen$variable=="Fem1"]
  }
  
  if (dim(val_indv)[1] == 0) {
    print("There is no usable bone measurement / indice available for the chosen formula")
  }
  
  return(val_indv)
}
