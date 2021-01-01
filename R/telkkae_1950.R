#' Calculate stature estimation according to:
#' Telkkä 1950.
#' 
#' @title telkkae_1950
#'
#' @description 
#' Stature estimation (mm) based on the mean of different regression calculations,
#' separated  by sex (Citation).
#' Bone measures used: Hum1, Rad2, Uln2, Fem1, Tib1, Fib1
#' 
#' If bone measures for left and right are provided the mean value will be used,
#' but for statistic information 2 bones will be counted (n_measures).
#' If sex is indet. the mean of male and female stature estimation is given.
#' To retrieve the estimated stature 20 mm will be substracted from the 
#' resulting mean value.
#'
#' Returns a data.frame with: 
#'     * ind: individual identifyer (rownames), 
#'     * sex: as provided for calculation: m, f, indet.
#'     * stature: estimated on the provided sex and bone measures, 
#'     * bone (measure(s)): bones used for calculation, 
#'     * female (stature): columns with alternative stature for three sex classes, 
#'     * male (stature), 
#'     * indet. (stature) and
#'     * n_measures: number of bone measures included: 
#'              e.g. 2 Fem2 (left, right) + 1 Tib1
#'
#' @param df data.frame containing informations on individual, bone and measurement.
#'  
#' @return data.frame with calculated stature and related information per individual.
#'           
#' @author Christoph Rinne <\email{crinne@@ufg.uni-kiel.de}>
#' 
#' @examples
#' 
#' @export

library(dplyr)

telkkae_1950 <- function(df){
  
  df$variable<-gsub("([rl]$)","", df$variable) # laterality not needed
  # aggregate values for each measure and individual
  df %>%  
    group_by(Ind, Sex, Group, variable) %>% 
    summarise(mean.value = mean(value), n = n()) -> df
  
  vec_indv <- unique(df$Ind) # extract names and quantity of unique individuals
  
  # Initialize data frame for later storage of different mean body heights
  val_indv <- as.data.frame(matrix(ncol=7, nrow=length(vec_indv)), row.names=vec_indv)
  colnames(val_indv) <-c("sex", "stature", "bone", "female", "male", "indet", "n_measures")
  val_indv$sex <- factor(val_indv$sex, labels = c("m", "f", "indet"), levels = c(1,2,3))
  
  # check available values for different variables needed for 
  for (i in 1:length(vec_indv)){
    df_knochen <- subset(df, subset=df$Ind == vec_indv[i])
    # Get measure values needed
    Hum1 <- df_knochen$mean.value[df_knochen$variable=="Hum1"]
    Rad2 <- df_knochen$mean.value[df_knochen$variable=="Rad2"]
    Uln2 <- df_knochen$mean.value[df_knochen$variable=="Uln2"]
    Fem1 <- df_knochen$mean.value[df_knochen$variable=="Fem1"]
    Tib1 <- df_knochen$mean.value[df_knochen$variable=="Tib1"]
    Fib1 <- df_knochen$mean.value[df_knochen$variable=="Fib1"]

    # document bone measures and number used for calculation
    
    bone <- c()
    n_measures <- 0
    if (length(Hum1)>0) {
      bone <- append(bone, "Hum1")
      n_measures <- n_measures + df_knochen$n[df_knochen$variable=="Hum1"]
    }
    if (length(Rad2)>0) {
      bone <- append(bone, "Rad2")
      n_measures <- n_measures + df_knochen$n[df_knochen$variable=="Rad2"]
    }
    if (length(Uln2)>0) {
      bone <- append(bone, "Uln2")
      n_measures <- n_measures + df_knochen$n[df_knochen$variable=="Uln2"]
    }
    if (length(Fem1)>0) {
      bone <- append(bone, "Fem1")
      n_measures <- n_measures + df_knochen$n[df_knochen$variable=="Fem1"]
    }
    if (length(Tib1)>0) {
      bone <- append(bone, "Tib1")
      n_measures <- n_measures + df_knochen$n[df_knochen$variable=="Tib1"]
    }
    if (length(Fib1)>0) {
      bone <- append(bone, "Fib1")
      n_measures <- n_measures + df_knochen$n[df_knochen$variable=="Fib1"]
    }

    # Calculate the different indices for male
    measures.m <- c()
    measures.m <- append (measures.m,  1694 + 2.8 * (Hum1 - 329))
    measures.m <- append (measures.m,  1694 + 3.4 * (Rad2 - 227))
    measures.m <- append (measures.m,  1694 + 3.2 * (Uln2 - 231))
    measures.m <- append (measures.m,  1694 + 2.1 * (Fem1 - 455))
    measures.m <- append (measures.m,  1694 + 2.1 * (Tib1 - 362))
    measures.m <- append (measures.m,  1694 + 2.5 * (Fib1 - 361))
    
    # Calculate the different indices for female 
    measures.f <- c()
    measures.f <- append (measures.f,  1568 + 2.7 * (Hum1 - 307))
    measures.f <- append (measures.f,  1568 + 3.1 * (Rad2 - 208))
    measures.f <- append (measures.f,  1568 + 3.3 * (Uln2 - 213))
    measures.f <- append (measures.f,  1568 + 1.8 * (Fem1 - 418))
    measures.f <- append (measures.f,  1568 + 1.9 * (Tib1 - 331))
    measures.f <- append (measures.f,  1568 + 2.3 * (Fib1 - 327))
    
    # Calculate the different indices for indet.
    measures.i <-(measures.m + measures.f)/2
    
    # calculate mean of each measures group for statures
    # as the regression equations are calculated in cm the result is converted to mm

    statures <- round(c(mean(measures.m), mean(measures.f), mean(measures.i)) - 20, 0)

    # write values into data frame of results
    val_indv$sex[i] <- unique(df_knochen$Sex)
    val_indv$stature[i] <- statures[as.integer(unique(df_knochen$Sex))]
    val_indv$bone <- paste(bone, collapse = ", ")
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
