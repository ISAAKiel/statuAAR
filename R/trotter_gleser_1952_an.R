#' Calculate stature estimation according to:
#' Trotter & Gleser 1952, 1977 series 'American Negro'.
#' 
#' @title trotter_gleser_1952an
#'
#' @description 
#' Stature estimation (mm) based on the hierarchy of different regression calculations,
#' separated  by sex (Trotter & Gleser 1952, 1977) series 'American Negro'.
#' Bone measures used: Fem1, Tib1b, Fib1, Uln1, Rad1, Hum1

#' If bone measures for left and right are provided the mean value will be used,
#' but for statistic information 2 bones will be counted (n_measures).
#' If sex is indet. the mean of male and female stature estimation is given.
#' The calculation is based on Trotter & Gleser (1952) with corrections from 
#' Trotter & Gleser (1977). The regression formula are hierarchical from 
#' combinations of different bone measures to single bone measures. Only the 
#' first applicable measure will be used.
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
#' @author Hendrik Raese <\email{h.raese@@ufg.uni-kiel.de}>
#' @author Christoph Rinne <\email{crinne@@ufg.uni-kiel.de}>
#' 
#' @examples
#' 
#'@export

library(dplyr)

trotter_gleser_1952an <- function(df){
  
  df$variable<-gsub("([rl]$)","", df$variable) # laterality not needed
  # aggregate values for each measure and individual
  options(dplyr.summarise.inform = FALSE)
  df %>%  
    group_by(Ind, Sex, Group, variable) %>% 
    summarise(mean.value = mean(value), n = n()) -> df
  
  vec_indv <- unique(df$Ind) # extract names and quantity of unique individuals
  
  # Initialize data frame for later storage of different mean body heights
  val_indv <- as.data.frame(matrix(ncol=8, nrow=length(vec_indv)), row.names=vec_indv)
  colnames(val_indv) <-c("sex", "group", "stature", "bone", "female", "male", "indet", "n_measures")
  val_indv$sex <- factor(val_indv$sex, labels = c("m", "f", "indet"), levels = c(1,2,3))
  
  # Calculte in hierarchical order
  
  # check available values for different variables needed for 
  for (i in 1:length(vec_indv)){
    df_bones <- subset(df, subset=df$Ind == vec_indv[i])
    
    # get all optional needed measures
    Fem1 <- df_bones$mean.value[df_bones$variable=="Fem1"]
    Tib1b <- df_bones$mean.value[df_bones$variable=="Tib1b"]
    Fib1 <- df_bones$mean.value[df_bones$variable=="Fib1"]
    Uln1 <- df_bones$mean.value[df_bones$variable=="Uln1"]
    Rad1 <- df_bones$mean.value[df_bones$variable=="Rad1"]
    Hum1 <- df_bones$mean.value[df_bones$variable=="Hum1"]
    
    # check for different combinations of measures
    # Fem1 & Tib1b
    if (length(Fem1)>0 & length(Tib1b)>0){
      stature.m <- ((Fem1 + Tib1b) * 1.30) + 632.9
      stature.f <- ((Fem1 + Tib1b) * 1.39) + 532.0
      statures <- c(stature.m, stature.f, mean(c(stature.m, stature.f)))
      indice <- "1. Fem1&Tib1b"
      n_measures <- df_bones$n[df_bones$variable=="Fem1"] + 
                    df_bones$n[df_bones$variable=="Tib1b"]
    # Fib1
    } else if (length(Fib1)>0) {
      stature.m <- (Fib1 * 2.68) + 717.8
      stature.f <- (Fib1 * 2.93) + 596.1
      statures <- c(stature.m, stature.f, mean(c(stature.m, stature.f)))
      indice <- "2. Fib1"
      n_measures <- df_bones$n[df_bones$variable=="Fib1"]
    # Tib1b
    } else if (length(Tib1b)>0) {
      stature.m <- (Tib1b * 2.52) + 786.2
      stature.f <- (Tib1b * 2.90) + 615.3
      statures <- c(stature.m, stature.f, mean(c(stature.m, stature.f)))
      indice <- "3. Tib1b"
      n_measures <- df_bones$n[df_bones$variable=="Tib1b"]
    # Fem1
    } else if (length(Fem1)>0) {
      stature.m <- (Fem1 * 2.38) + 614.1
      stature.f <- (Fem1 * 2.47) + 541.0
      statures <- c(stature.m, stature.f, mean(c(stature.m, stature.f)))
      indice <- "4. Fem1"
      n_measures <- df_bones$n[df_bones$variable=="Fem1"]
    # Uln1
    } else if (length(Uln1)>0) {
      stature.m <- (Uln1 * 3.70) + 740.5
      stature.f <- (Uln1 * 4.27) + 577.6
      statures <- c(stature.m, stature.f, mean(c(stature.m, stature.f)))
      indice <- "5. Uln1"
      n_measures <- df_bones$n[df_bones$variable=="Uln1"]
    # Rad1
    } else if (length(Rad1)>0) {
      stature.m <- (Rad1 * 3.78) + 790.1
      stature.f <- (Rad1 * 4.74) + 549.3
      statures <- c(stature.m, stature.f, mean(c(stature.m, stature.f)))
      indice <- "6. Rad1"
      n_measures <- df_bones$n[df_bones$variable=="Rad1"]
    } else if (length(Hum1)>0) {
      stature.m <- (Hum1 * 3.08) + 704.5
      stature.f <- (Hum1 * 3.36) + 579.7
      statures <- c(stature.m, stature.f, mean(c(stature.m, stature.f)))
      indice <- "7. Hum1"
      n_measures <- df_bones$n[df_bones$variable=="Hum1"]
    } else {
    # no apropriate measures given
      statures <-rep(NA, 3)
      indice <- NA
      n_measures <- 0
    }
    
    statures <- round(statures, 0)
    
    # write values into data frame of results
    val_indv$sex[i] <- unique(df_bones$Sex)
    val_indv$stature[i] <- statures[as.integer(unique(df_bones$Sex))]
    val_indv$bone[i] <- indice
    val_indv$group[i] <- unique(df_bones$Group)
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
