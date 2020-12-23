#' Calculate body height indices and mean body height based on Vercellotti et al. 2009
#' 
#' Based on the available measurements of different bones of different individuals, the body height indices for males and females are calculated according to Vercelotti et al. 2009.
#'
#' 
#' @title vercellotti_et_al_2009
#'
#' @description 
#' Based on the available measurements of different bones of 
#' different individuals, the body height indices for  
#' males and females are calculated according to Vercelotti et al. 2009.
#' Returns a data.frame with: 
#'     * Ind: Individual identifyer (rownames), 
#'     * Sex: 
#'     * Statuar: estimated on the provided sex and bone measures, 
#'     * Bone (measure(s)): bones used for calculation, 
#'     * female (statuar): columns with alternative statuar for three sex classes, 
#'     * male (statuar), 
#'     * indet. (statuar)
#' 
#' @param df data.frame, containing informations on individual, bone and measurement
#'  
#' @return data.frame, containing one data.frame with all calculated indices for every individual
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

calc.Fem2.Tib1 <- function(df_knochen){
  a <- df_knochen$mean.value[df_knochen$variable == "Fem2"] +
    df_knochen$mean.value[df_knochen$variable == "Tib1"]
  statuar.m <- (a * 1.50) + 469
  statuar.f <- (a * 1.55) + 390
  statuar.i <- (statuar.m + statuar.f)/2
  statuars <- c(statuar.m, statuar.f, statuar.i)
  return (round(statuars, 0))
}

calc.Fem2 <- function(df_knochen) {
  a <- df_knochen$mean.value[df_knochen$variable == "Fem2"]
  statuar.m <- (a * 2.70) + 481
  statuar.f <- (a * 2.89) + 365
  statuar.i <- (statuar.m + statuar.f)/2
  statuars <- c(statuar.m, statuar.f, statuar.i)
  return (round(statuars, 0))
}

calc.Fem1 <- function(df_knochen) {
  a <- df_knochen$mean.value[df_knochen$variable == "Fem1"]
  statuar.m <- (a * 2.61) + 515
  statuar.f <- (a * 2.89) + 353
  statuar.i <- (statuar.m + statuar.f)/2
  statuars <- c(statuar.m, statuar.f, statuar.i)
  return (round(statuars, 0))
}

calc.Tib1 <- function(df_knochen) {
  a <- df_knochen$mean.value[df_knochen$variable == "Tib1"]
  statuar.m <- (a * 2.91) + 631
  statuar.f <- (a * 2.79) + 614
  statuar.i <- (statuar.m + statuar.f)/2
  statuars <- c(statuar.m, statuar.f, statuar.i)
  return (round(statuars, 0))
}

calc.Hum1 <- function(df_knochen) {
  a <- df_knochen$mean.value[df_knochen$variable == "Hum1"]
  statuar.m <- (a * 3.11) + 677
  statuar.f <- (a * 3.11) + 630
  statuar.i <- (statuar.m + statuar.f)/2
  statuars <- c(statuar.m, statuar.f, statuar.i)
  return (round(statuars, 0))
}

calc.Hum1 <- function(df_knochen) {
  a <- df_knochen$mean.value[df_knochen$variable == "Rad1"]
  statuar.m <- (a * 1.92) + 1230
  statuar.f <- (a * 3.45) + 785
  statuar.i <- (statuar.m + statuar.f)/2
  statuars <- c(statuar.m, statuar.f, statuar.i)
  return (round(statuars, 0))
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
  colnames(val_indv) <-c("sex", "statuar", "bone", "female", "male", "indet", "n_measures")
  val_indv$sex <- factor(val_indv$sex, levels = c("m", "f", "i"))
  
  # check available values for different variables needed for 
  for (i in 1:length(vec_indv)){
    df_knochen <- subset(df, subset=df$Ind == vec_indv[i])
    # check for different combinations of measures
    # Fem2 & Tib1
    if (length(df_knochen$mean.value[df_knochen$variable == "Fem2"])>0 & 
        length(df_knochen$mean.value[df_knochen$variable == "Tib1"])>0){
      statuars <- calc.Fem2.Tib1(df_knochen) # function to do caclculation
      indice <- "Fem2&Tib1"
      n_measures <- df_knochen$n[df_knochen$variable=="Fem2"] + n[df_knochen$variable=="Tib1"]
    # Fem2
    } else if (length(df_knochen$mean.value[df_knochen$variable == "Fem2"])>0) {
      statuars <- calc.Fem2(df_knochen) # function to do caclculation
      indice <- "Fem2"
      n_measures <- df_knochen$n[df_knochen$variable=="Fem2"]
    # Fem1
    } else if (is.numeric(df_knochen$mean.value[df_knochen$variable == "Fem1"])>0) {
      statuars <- calc.Fem1(df_knochen) # function to do caclculation
      indice <- "Fem1"
      n_measures <- df_knochen$n[df_knochen$variable=="Fem1"]
    # Tib1
    } else if (length(df_knochen$mean.value[df_knochen$variable == "Tib1"])>0) {
      statuars <- calc.Tib1(df_knochen) # function to do caclculation
      indice <- "Tib1"
      n_measures <- df_knochen$n[df_knochen$variable=="Tib1"]
    # Hum1
    } else if (length(df_knochen$mean.value[df_knochen$variable == "Hum1"])>0) {
      statuars <- calc.Hum1(df_knochen) # function to do caclculation
      indice <- "Tib1"
      n_measures <- df_knochen$n[df_knochen$variable=="Hum1"]
    # Rad1
    } else if (length(df_knochen$mean.value[df_knochen$variable == "Rad1"])>0) {
      statuars <- calc.Rad1(df_knochen) # function to do caclculation
      indice <- "Rad1"
      n_measures <- df_knochen$n[df_knochen$variable=="Rad1"]
    } else {
    # no apropriate measures given
      statuars <-rep(NA, 3)
      indice <- NA
      n_measures <- 0
    }
    
    # write values into data frame of results
    val_indv$sex[i] <- unique(df_knochen$Sex)
    val_indv$statuar[i] <- statuars[as.integer(unique(df_knochen$Sex))]
    val_indv$bone[i] <- indice
    val_indv$female[i] <- statuars[2]
    val_indv$male[i] <- statuars[1]
    val_indv$indet[i] <- statuars[3]
    val_indv$n_measures[i] <- n_measures
  }
  
  if (dim(val_indv)[1] == 0) {
    print("There is no usable bone measurement / indice available for the chosen formula")
  }
  
  #rm(i, vec_indv)

  return(val_indv)
}
