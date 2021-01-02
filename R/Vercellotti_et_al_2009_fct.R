#' Calculate stature estimation according to:
#' Vercellotti et al. 2009
#' 
#' @title vercellotti_et_al_2009
#'
#' @description 
#' Stature estimation (mm) based on the hierarchy of different regression calculations,
#' separated  by sex (Vercellotti et al. 2009).
#' Bone measures used: Fem2+Tib1, Fem2, Fem1, Tib1, Hum1+Rad1, Hum1, Rad1
#' 
#' If bone measures for left and right are provided the mean value will be used,
#' but for statistic information 2 bones will be counted (n_measures).
#' Vercellotti propose regression calculations for the combination of male and female
#' individuals. These regressions will be used in case of undetermined sex (indet.).
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

#######################################################
calc.stature.m <- function (df_bones){
  
  # get all optional needed measures
  Fem1 <- df_bones$mean.value[df_bones$variable=="Fem1"]
  Fem2 <- df_bones$mean.value[df_bones$variable=="Fem2"]
  Tib1 <- df_bones$mean.value[df_bones$variable=="Tib1"]
  Hum1 <- df_bones$mean.value[df_bones$variable=="Hum1"]
  Rad1 <- df_bones$mean.value[df_bones$variable=="Rad1"]
  
  stature.m<-c()
  
  stature.m <- ((Fem2 + Tib1) * 1.5) + 469
  indice <- "Fem2+Tib1"
  n_measures <- df_bones$n[df_bones$variable=="Fem1"] + 
    df_bones$n[df_bones$variable=="Tib1"]
  if (length(stature.m)==0){
    stature.m <- (Fem2 * 2.7) + 481
    indice <- "Fem2"
    n_measures <- df_bones$n[df_bones$variable=="Fem2"]
  } 
  if (length(stature.m)==0){
    stature.m <- (Fem1 * 2.61) + 515
    indice <- "Fem1"
    n_measures <- df_bones$n[df_bones$variable=="Fem1"]
  } 
  if (length(stature.m)==0){
    stature.m <- (Tib1 * 2.91) + 631
    indice <- "Tib1"
    n_measures <- df_bones$n[df_bones$variable=="Tib1"]
  } 
  if (length(stature.m)==0){
    stature.m <- (Hum1 * 3.11) + 677
    indice <- "Hum1"
    n_measures <- df_bones$n[df_bones$variable=="Hum1"]
  } 
  if (length(stature.m)==0){
    stature.m <- ((Hum1 + Rad1) * 1.51) + 829
    indice <- "Hum1&Rad1"
    n_measures <- df_bones$n[df_bones$variable=="Rad1"] + 
      df_bones$n[df_bones$variable=="Hum1"] 
  } 
  if (length(stature.m)==0){
    stature.m <- (Rad1 * 1.92) + 123
    indice <- "Rad1"
    n_measures <- df_bones$n[df_bones$variable=="Rad1"]
  } 
  # End of male stature estimation
  
  return(list("stature"=stature.m, "indice"=indice, "n_measures"=n_measures))
  
} # End of Function calc.stature.m

##############################################################

calc.stature.f <- function (df_bones){
  
  # get all optional needed measures
  Fem1 <- df_bones$mean.value[df_bones$variable=="Fem1"]
  Fem2 <- df_bones$mean.value[df_bones$variable=="Fem2"]
  Tib1 <- df_bones$mean.value[df_bones$variable=="Tib1"]
  Hum1 <- df_bones$mean.value[df_bones$variable=="Hum1"]
  Rad1 <- df_bones$mean.value[df_bones$variable=="Rad1"]
  
  stature.f<-c()
  
  stature.f <- (Fem2 * 2.89) + 365
  indice <- "Fem2"
  n_measures <- df_bones$n[df_bones$variable=="Fem2"]
  if (length(stature.f)==0){
    stature.f <- ((Fem2+Tib1) * 1.55) + 390
    indice <- "Fem2&Tib1"
    n_measures <- df_bones$n[df_bones$variable=="Tib1"] + 
      df_bones$n[df_bones$variable=="Fem2"]
  } 
  if (length(stature.f)==0){
    stature.f <- (Fem1 * 2.89) + 353
    indice <- "Fem1"
    n_measures <- df_bones$n[df_bones$variable=="Fem1"]
  } 
  if (length(stature.f)==0){
    stature.f <- (Tib1 * 2.79) + 614
    indice <- "Tib1"
    n_measures <- df_bones$n[df_bones$variable=="Tib1"]
  } 
  if (length(stature.f)==0){
    stature.f <- (Hum1 * 3.11) + 630
    indice <- "Hum1"
    n_measures <- df_bones$n[df_bones$variable=="Hum1"]
  } 
  if (length(stature.f)==0){
    stature.f <- ((Hum1 + Rad1) * 1.72) + 657
    indice <- "Hum1&Rad1"
    n_measures <- df_bones$n[df_bones$variable=="Hum1"] +
      df_bones$n[df_bones$variable=="Rad1"]
  } 
  if (length(stature.f)==0){
    stature.f <- (Rad1 * 3.45) + 785
    indice <- "Rad1"
    n_measures <- df_bones$n[df_bones$variable=="Rad1"] 
  } 
  # End of male stature estimation
  
  return(list("stature"=stature.f, "indice"=indice, "n_measures"=n_measures))
  
} # End of Function calc.stature.f


###################################

calc.stature.i <- function (df_bones){
  
  # get all optional needed measures
  Fem1 <- df_bones$mean.value[df_bones$variable=="Fem1"]
  Fem2 <- df_bones$mean.value[df_bones$variable=="Fem2"]
  Tib1 <- df_bones$mean.value[df_bones$variable=="Tib1"]
  Hum1 <- df_bones$mean.value[df_bones$variable=="Hum1"]
  Rad1 <- df_bones$mean.value[df_bones$variable=="Rad1"]
  
  stature.i<-c()
  
  stature.i <- (Fem2 * 3.10) + 292
  indice <- "Fem2"
  n_measures <- df_bones$n[df_bones$variable=="Fem2"]
  if (length(stature.i)==0){
    stature.i <- ((Fem2+Tib1) * 1.74) + 265
    indice <- "Fem2&Tib1"
    n_measures <- df_bones$n[df_bones$variable=="Tib1"] + 
      df_bones$n[df_bones$variable=="Fem2"]
  } 
  if (length(stature.i)==0){
    stature.i <- (Fem1 * 3.10) + 281
    indice <- "Fem1"
    n_measures <- df_bones$n[df_bones$variable=="Fem1"]
  } 
  if (length(stature.i)==0){
    stature.i <- (Tib1 * 3.58) + 366
    indice <- "Tib1"
    n_measures <- df_bones$n[df_bones$variable=="Tib1"]
  } 
  if (length(stature.i)==0){
    stature.i <- (Hum1 * 3.76) + 451
    indice <- "Hum1"
    n_measures <- df_bones$n[df_bones$variable=="Hum1"]
  } 
  if (length(stature.i)==0){
    stature.i <- ((Hum1 + Rad) * 1.93) + 567
    indice <- "Hum1&Rad1"
    n_measures <- df_bones$n[df_bones$variable=="Hum1"] +
      df_bones$n[df_bones$variable=="Rad1"]
  } 
  if (length(stature.i)==0){
    stature.i <- (Rad1 * 3.18) + 886
    indice <- "Rad1"
    n_measures <- df_bones$n[df_bones$variable=="Rad1"] 
  } 
  # End of male stature estimation
  
  return(list("stature"=stature.i, "indice"=indice, "n_measures"=n_measures))
  
} # End of Function calc.stature.f

###################################

vercellotti_etal_2009 <- function(df){
  
  df$variable<-gsub("([rl]$)","", df$variable) # laterality not needed
  # aggregate values for each measure and individual
  df %>%  
    group_by(Ind, Sex, Group, variable) %>% 
    summarise(mean.value = mean(value), n = n()) -> df
  
  vec_indv <- unique(df$Ind) # extract names and quantity of unique individuals
  
  # Initialize data frame for later storage of different mean body heights
  val_indv<- as.data.frame(matrix(ncol=7, nrow=length(vec_indv)), row.names=vec_indv)
  colnames(val_indv) <-c("sex", "stature", "bone", "female", "male", "indet", "n_measures")
  val_indv$sex <- factor(val_indv$sex, labels = c("m", "f", "indet"), levels = c(1,2,3))
  
  
  # check available values for different variables needed for 
  for (i in 1:length(vec_indv)){
    df_bones <- subset(df, subset = Ind==vec_indv[i])
    
    stature.m <- calc.stature.m(df_bones)
    stature.f <- calc.stature.f(df_bones)
    stature.i <- calc.stature.i(df_bones)
    
    # vectors for stature estimations, indices and n_measures
    statures <- c(stature.m[[1]], stature.f[[1]], stature.i[[1]])
    statures <- round(statures, 0)
    indices <- c(stature.m[[2]], stature.f[[2]], stature.i[[2]])
    n_measures <- c(stature.m[[3]], stature.f[[3]], stature.i[[3]])
    
    # write values into data frame of results
    val_indv$sex[i] <- unique(df_bones$Sex)
    val_indv$stature[i] <- statures[as.integer(unique(df_bones$Sex))]
    val_indv$bone[i] <- indices[as.integer(unique(df_bones$Sex))]
    val_indv$female[i] <- statures[2]
    val_indv$male[i] <- statures[1]
    val_indv$indet[i] <- statures[3]
    val_indv$n_measures[i] <- n_measures[as.integer(unique(df_bones$Sex))]
  } # next individual
  
  if (dim(val_indv)[1] == 0) {
    print("There is no usable bone measurement / indice available for the chosen formula")
  }
  
  #rm(i, vec_indv)

  return(val_indv)
}
