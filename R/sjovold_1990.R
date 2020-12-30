#' Calculate body height indices and mean body height based on Sjøvold 1990.
#' 
#' Based on the available measurements of different bones of different individuals, 
#' the body height indices for males and females are calculated.
#'
#' 
#' @title sjovold_1990
#'
#' @description 
#' Based on the available measurements of different bones of 
#' different individuals, the body height indices for  
#' males and females are calculated.
#' The average length from bones of both sides are calculated.  
#' The regression formula of Sjøvold (1990) calculate the body length. Allthough
#' not explained in the text estimated stature is derived from the mean of all 
#' bones. To not multiply significance i the case of two measures per bone 
#' (e.g. Fem 1, Fem2) only one of both will be used.
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

sjovold_1990 <- function(df){
  
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
  val_indv$sex <- factor(val_indv$sex, levels = c("m", "f", "i"))
  
  # check available values for different variables needed for 
  for (i in 1:length(vec_indv)){
    df_knochen <- subset(df, subset=df$Ind == vec_indv[i])
    # Get measure values needed
    Hum1 <- df_knochen$mean.value[df_knochen$variable=="Hum1"]
    Rad1 <- df_knochen$mean.value[df_knochen$variable=="Rad1"]
    Rad1b <- df_knochen$mean.value[df_knochen$variable=="Rad1b"]
    Uln1 <- df_knochen$mean.value[df_knochen$variable=="Uln1"]
    Fem1 <- df_knochen$mean.value[df_knochen$variable=="Fem1"]
    Fem2 <- df_knochen$mean.value[df_knochen$variable=="Fem2"]
    Tib1 <- df_knochen$mean.value[df_knochen$variable=="Tib1"]
    Tib1b <- df_knochen$mean.value[df_knochen$variable=="Tib1b"]
    Fib1 <- df_knochen$mean.value[df_knochen$variable=="Fib1"]
    
    # calculate statuar and document bone measures and number used for calculation
    
    measures <- c()
    bone <- c()
    n_measures <- 0
    if (length(Hum1)>0){
      measures <- append(measures, (4.62 * Hum1) + 190.00)
      bone <- append(bone, "Hum1")
      n_measures <- n_measures + df_knochen$n[df_knochen$variable=="Hum1"]
    }
    if (length(Rad1)>0) {
      measures <- append(measures, (3.78 * Rad1) + 747.0)
      bone <- append(bone, "Rad1")
      n_measures <- n_measures + df_knochen$n[df_knochen$variable=="Rad1"]
    }else if (length(Rad1b)>0){
      measures <- append(measures, (4.80 * Rad1b) + 515.5)
      bone <- append(bone, "Rad1")
      n_measures <- n_measures + df_knochen$n[df_knochen$variable=="Rad1b"]
    }
    if (length(Uln1)>0) {
      measures <- append(measures, (4.61 * Uln1) + 468.3)
      bone <- append(bone, "Uln1")
      n_measures <- n_measures + df_knochen$n[df_knochen$variable=="Uln1"]
    }
    if (length(Fem1)>0) {
      measures <- append(measures, (2.71 * Fem1) + 458.6)
      bone <- append(bone, "Fem1")
      n_measures <- n_measures + df_knochen$n[df_knochen$variable=="Fem1"]
    } else if (length(Fem2)>0){
      measures <- append(measures, (3.01 * Fem2) + 325.2)
      bone <- append(bone, "Fem2")
      n_measures <- n_measures + df_knochen$n[df_knochen$variable=="Fem2"]
    }
    if (length(Tib1)>0) {
      measures <- append(measures, (3.29 * Tib1) + 473.4)
      bone <- append(bone, "Tib1")
      n_measures <- n_measures + df_knochen$n[df_knochen$variable=="Tib1"]
    } else if (length(Tib1b)>0){
      measures <- append((3.67 * Tib1b) + 295.0)
      bone <- append(bone, "Tib1b")
      n_measures <- n_measures + df_knochen$n[df_knochen$variable=="Tib1b"]
    }
    if (length(Fib1)>0) {
      measures <- append(measures, (3.59 * Fib2) + 363.1)
      bone <- append(bone, "Fib1")
      n_measures <- n_measures + df_knochen$n[df_knochen$variable=="Fib1"]
    }

    # calculate mean of each measures group for statures
    # as the regression equations are calculated in cm the result is converted to mm

    stature <- round(mean(measures), 0)

    # write values into data frame of results
    val_indv$sex[i] <- unique(df_knochen$Sex)
    val_indv$stature[i] <- stature
    val_indv$bone <- paste(bone, collapse = ", ")
    val_indv$female[i] <- stature
    val_indv$male[i] <- stature
    val_indv$indet[i] <- stature
    val_indv$n_measures[i] <- n_measures
  }
  
  if (dim(val_indv)[1] == 0) {
    print("There is no usable bone measurement / indice available for the chosen formula")
  }
  
  return(val_indv)
}
