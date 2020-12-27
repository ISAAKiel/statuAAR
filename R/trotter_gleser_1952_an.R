#' Calculate body height indices and mean body height based on Trotter & Gleser 1952, 1977 series 'American Negro'.
#' 
#' Based on the available measurements of different bones of different individuals, the body height indices for males and females are calculated according to Vercelotti et al. 2009.
#'
#' 
#' @title trotter_gleser_1952aw
#'
#' @description 
#' Based on the available measurements of different bones of 
#' different individuals, the body height indices for  males and females are 
#' calculated according to Trotter & Gleser (1952) with corrections from 
#' Trotter & Gleser (1977). Formula for the series 'American Negro'.
#' The regression formula are hierarchical from combinations of different 
#' bone measures to single bone measures. Only the first applicable measure will 
#' be used.
#' 
#' Returns a data.frame with: 
#'     * Ind: Individual identifyer (rownames), 
#'     * Sex: 
#'     * Stature: estimated on the provided sex and bone measures, 
#'     * Bone (measure(s)): bones used for calculation, 
#'     * female (stature): columns with alternative stature for three sex classes, 
#'     * male (stature), 
#'     * indet. (stature)
#' 
#' @param df data.frame, containing informations on individual, bone and measurement
#'  
#' @return data.frame, containing one data.frame with all calculated indices for every individual
#'           
#' @author Hendrik Raese <\email{h.raese@@ufg.uni-kiel.de}>
#' @author Christoph Rinne <\email{crinne@@ufg.uni-kiel.de}>
#' 
#' @examples
#' 
#'@export

library(dplyr)

trotter_gleser_1952aw <- function(df){
  
  df$variable<-gsub("([rl]$)","", df$variable) # laterality not needed
  # aggregate values for each measure and individual
  df %>%  
    group_by(Ind, Sex, Group, variable) %>% 
    summarise(mean.value = mean(value), n = n()) -> df
  
  vec_indv <- unique(df$Ind) # extract names and quantity of unique individuals
  
  # Initialize data frame for later storage of different mean body heights
  val_indv <- as.data.frame(matrix(ncol=7, nrow=length(vec_indv)), row.names=vec_indv)
  colnames(val_indv) <-c("sex", "stature", "bone", "female", "male", "indet", "n_measures")
  val_indv$sex <- factor(val_indv$sex, levels = c("m", "f", "i"))
  
  # Calculte in hierarchical order
  
  # check available values for different variables needed for 
  for (i in 1:length(vec_indv)){
    df_knochen <- subset(df, subset=df$Ind == vec_indv[i])
    
    # get all optional needed measures
    Fem1 <- df_knochen$mean.value[df_knochen$variable=="Fem1"]
    Tib1b <- df_knochen$mean.value[df_knochen$variable=="Tib1b"]
    Fib1 <- df_knochen$mean.value[df_knochen$variable=="Fib1"]
    Uln1 <- df_knochen$mean.value[df_knochen$variable=="Uln1"]
    Rad1 <- df_knochen$mean.value[df_knochen$variable=="Rad1"]
    Hum1 <- df_knochen$mean.value[df_knochen$variable=="Hum1"]
    
    # check for different combinations of measures
    # Fem1 & Tib1b
    if (length(Fem1)>0 & length(Tib1b)>0){
      stature.m <- ((Fem1 + Tib1b) * 1.30) + 632.9
      stature.f <- ((Fem1 + Tib1b) * 1.39) + 532.0
      statures <- c(stature.m, stature.f, mean(c(stature.m, stature.f)))
      indice <- "1. Fem1&Tib1b"
      n_measures <- df_knochen$n[df_knochen$variable=="Fem1"] + 
                    df_knochen$n[df_knochen$variable=="Tib1b"]
    # Fib1
    } else if (length(Fib1)>0) {
      stature.m <- (Fib1 * 2.68) + 717.8
      stature.f <- (Fib1 * 2.93) + 596.1
      statures <- c(stature.m, stature.f, mean(c(stature.m, stature.f)))
      indice <- "2. Fib1"
      n_measures <- df_knochen$n[df_knochen$variable=="Fib1"]
    # Tib1b
    } else if (length(Tib1b)>0) {
      stature.m <- (Tib1b * 2.52) + 786.2
      stature.f <- (Tib1b * 2.90) + 615.3
      statures <- c(stature.m, stature.f, mean(c(stature.m, stature.f)))
      indice <- "3. Tib1b"
      n_measures <- df_knochen$n[df_knochen$variable=="Tib1b"]
    # Fem1
    } else if (length(Fem1)>0) {
      stature.m <- (Fem1 * 2.38) + 614.1
      stature.f <- (Fem1 * 2.47) + 541.0
      statures <- c(stature.m, stature.f, mean(c(stature.m, stature.f)))
      indice <- "4. Fem1"
      n_measures <- df_knochen$n[df_knochen$variable=="Fem1"]
    # Uln1
    } else if (length(Uln1)>0) {
      stature.m <- (Uln1 * 3.70) + 740.5
      stature.f <- (Uln1 * 4.27) + 577.6
      statures <- c(stature.m, stature.f, mean(c(stature.m, stature.f)))
      indice <- "5. Uln1"
      n_measures <- df_knochen$n[df_knochen$variable=="Uln1"]
    # Rad1
    } else if (length(Rad1)>0) {
      stature.m <- (Rad1 * 3.78) + 790.1
      stature.f <- (Rad1 * 4.74) + 549.3
      statures <- c(stature.m, stature.f, mean(c(stature.m, stature.f)))
      indice <- "6. Rad1"
      n_measures <- df_knochen$n[df_knochen$variable=="Rad1"]
    } else if (length(Hum1)>0) {
      stature.m <- (Hum1 * 3.08) + 704.5
      stature.f <- (Hum1 * 3.36) + 579.7
      statures <- c(stature.m, stature.f, mean(c(stature.m, stature.f)))
      indice <- "7. Hum1"
      n_measures <- df_knochen$n[df_knochen$variable=="Hum1"]
    } else {
    # no apropriate measures given
      statures <-rep(NA, 3)
      indice <- NA
      n_measures <- 0
    }
    
    statures <- round(statures, 0)
    
    # write values into data frame of results
    val_indv$sex[i] <- unique(df_knochen$Sex)
    val_indv$stature[i] <- statures[as.integer(unique(df_knochen$Sex))]
    val_indv$bone[i] <- indice
    val_indv$female[i] <- statures[2]
    val_indv$male[i] <- statures[1]
    val_indv$indet[i] <- statures[3]
    val_indv$n_measures[i] <- n_measures
  }
  
  if (dim(val_indv)[1] == 0) {
    print("There is no usable bone measurement / indice available for the chosen formula")
  }
  
  #rm(i, vec_indv)

  return(val_indv)
}
