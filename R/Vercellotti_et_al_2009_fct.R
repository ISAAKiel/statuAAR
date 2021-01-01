#' Calculate stature estimation according to:
#' Vercellotti et al. 2009
#' 
#' @title vercellotti_et_al_2009
#'
#' @description 
#' Stature estimation (mm) based on the hierarchy of different regression calculations,
#' separated  by sex (Vercellotti et al. 2009).
#' Bone measures used: Fem2+Tib1, Fem2, Fem1, Tib1, Hum1, Rad1
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
#' @author Anna Loy <\email{aloy@@roots.uni-kiel.de}>
#' @author Nils Müller-Scheeßel <\email{nils.mueller-scheessel@@ufg.uni-kiel.de}>
#' @author Hendrik Raese <\email{h.raese@@ufg.uni-kiel.de}>
#' @author Christoph Rinne <\email{crinne@@ufg.uni-kiel.de}>
#' 
#' @examples
#' 
#'@export

library(dplyr)

calc.Fem2.Tib1 <- function(df_bones){
  a <- df_bones$mean.value[df_bones$variable == "Fem2"] +
    df_bones$mean.value[df_bones$variable == "Tib1"]
  stature.m <- (a * 1.50) + 469
  stature.f <- (a * 1.55) + 390
  stature.i <- (stature.m + stature.f)/2
  statures <- c(stature.m, stature.f, stature.i)
  return (round(statures, 0))
}

calc.Fem2 <- function(df_bones) {
  a <- df_bones$mean.value[df_bones$variable == "Fem2"]
  stature.m <- (a * 2.70) + 481
  stature.f <- (a * 2.89) + 365
  stature.i <- (stature.m + stature.f)/2
  statures <- c(stature.m, stature.f, stature.i)
  return (round(statures, 0))
}

calc.Fem1 <- function(df_bones) {
  a <- df_bones$mean.value[df_bones$variable == "Fem1"]
  stature.m <- (a * 2.61) + 515
  stature.f <- (a * 2.89) + 353
  stature.i <- (stature.m + stature.f)/2
  statures <- c(stature.m, stature.f, stature.i)
  return (round(statures, 0))
}

calc.Tib1 <- function(df_bones) {
  a <- df_bones$mean.value[df_bones$variable == "Tib1"]
  stature.m <- (a * 2.91) + 631
  stature.f <- (a * 2.79) + 614
  stature.i <- (stature.m + stature.f)/2
  statures <- c(stature.m, stature.f, stature.i)
  return (round(statures, 0))
}

calc.Hum1 <- function(df_bones) {
  a <- df_bones$mean.value[df_bones$variable == "Hum1"]
  stature.m <- (a * 3.11) + 677
  stature.f <- (a * 3.11) + 630
  stature.i <- (stature.m + stature.f)/2
  statures <- c(stature.m, stature.f, stature.i)
  return (round(statures, 0))
}

calc.Rad1 <- function(df_bones) {
  a <- df_bones$mean.value[df_bones$variable == "Rad1"]
  stature.m <- (a * 1.92) + 1230
  stature.f <- (a * 3.45) + 785
  stature.i <- (stature.m + stature.f)/2
  statures <- c(stature.m, stature.f, stature.i)
  return (round(statures, 0))
}

vercellotti_et_al_2009 <- function(df){
  
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
    df_bones <- subset(df, subset=df$Ind == vec_indv[i])
    # check for different combinations of measures
    # Fem2 & Tib1
    if (length(df_bones$mean.value[df_bones$variable == "Fem2"])>0 & 
        length(df_bones$mean.value[df_bones$variable == "Tib1"])>0){
      statures <- calc.Fem2.Tib1(df_bones) # function to do caclculation
      indice <- "Fem2&Tib1"
      n_measures <- df_bones$n[df_bones$variable=="Fem2"] + 
        df_bones$n[df_bones$variable=="Tib1"]
    # Fem2
    } else if (length(df_bones$mean.value[df_bones$variable == "Fem2"])>0) {
      statures <- calc.Fem2(df_bones) # function to do caclculation
      indice <- "Fem2"
      n_measures <- df_bones$n[df_bones$variable=="Fem2"]
    # Fem1
    } else if (length(df_bones$mean.value[df_bones$variable == "Fem1"])>0) {
      statures <- calc.Fem1(df_bones) # function to do caclculation
      indice <- "Fem1"
      n_measures <- df_bones$n[df_bones$variable=="Fem1"]
    # Tib1
    } else if (length(df_bones$mean.value[df_bones$variable == "Tib1"])>0) {
      statures <- calc.Tib1(df_bones) # function to do caclculation
      indice <- "Tib1"
      n_measures <- df_bones$n[df_bones$variable=="Tib1"]
    # Hum1
    } else if (length(df_bones$mean.value[df_bones$variable == "Hum1"])>0) {
      statures <- calc.Hum1(df_bones) # function to do caclculation
      indice <- "Tib1"
      n_measures <- df_bones$n[df_bones$variable=="Hum1"]
    # Rad1
    } else if (length(df_bones$mean.value[df_bones$variable == "Rad1"])>0) {
      statures <- calc.Rad1(df_bones) # function to do caclculation
      indice <- "Rad1"
      n_measures <- df_bones$n[df_bones$variable=="Rad1"]
    } else {
    # no apropriate measures given
      statures <-rep(NA, 3)
      indice <- NA
      n_measures <- 0
    }
    
    # write values into data frame of results
    val_indv$sex[i] <- unique(df_bones$Sex)
    val_indv$stature[i] <- statures[as.integer(unique(df_bones$Sex))]
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
