#' @name pearson_1899
#'
#' @title Calculate stature estimation according to: Pearson 1899
#' 
#' @description 
#' Stature estimation (mm) based on the mean of several multiple and single
#' regression calculations, separated  by sex (Pearson 1899).
#' Bone measures used: Hum1, Rad1, Fem1, Tib1b, Tib1a
#' 
#' If bone measures for left and right are provided the mean value will be used,
#' but for statistic information 2 bones will be counted (n_measures).
#' If sex is indet. the mean of male and female stature estimation is given.
#' According to Pearson (1899, 196-97) following measure substitudes will be used:
#'   male: Fem1 = Fem2 + 3.2 mm, Tib1b = Tib1a - 9.6 mm
#'   female: Fem1 = Fem2 + 3.3 mm, Tib1b = Tib1a - 8.7 mm 
#' Pearson's work is based on the bones from the right side. According to 
#' measurements from Rollet (1888) he provides correction values for the bones of
#' the left side. These values vary for male within +/- .4 - 4.2 mm and for 
#' female within +/- 0.4 - 5.1 mm. Leg length discrepancy (LLD) is a complex 
#' process due to multiple causes with environmental and genetic factors and 
#' increasing with age. Based on French data minor LLD is often, but if treated 
#' in most cases below 2 cm per leg. In addition differences between sex and  
#' left vs right side are observed (Guichet et al 1991, Holliday & Ruff 2001, 
#' Knutson 2005). In consequence a compounded correction between left and right 
#' bones by the values derived from the sample of Rollet (1888) is rejected.  
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
#'@export

library(dplyr)

pearson_1899 <- function(df){
  
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
    # Get measure values needed
    Hum1 <- df_bones$mean.value[df_bones$variable=="Hum1"]
    Rad1 <- df_bones$mean.value[df_bones$variable=="Rad1"]
    Fem1 <- df_bones$mean.value[df_bones$variable=="Fem1"]
    Fem2 <- df_bones$mean.value[df_bones$variable=="Fem1"]
    Tib1b <- df_bones$mean.value[df_bones$variable=="Tib1b"]
    Tib1a <- df_bones$mean.value[df_bones$variable=="Tib1a"]

    # document bone measures and number used for calculation 
    bone <- c()
    n_measures <- 0
    if (length(Hum1)>0) {
      bone <- append(bone, "Hum1")
      n_measures <- n_measures + df_bones$n[df_bones$variable=="Hum1"]
    }
    if (length(Rad1)>0) {
      bone <- append(bone, "Rad1")
      n_measures <- n_measures + df_bones$n[df_bones$variable=="Rad1"]
    }
    if (length(Fem1)>0) {
      bone <- append(bone, "Fem1")
      n_measures <- n_measures + df_bones$n[df_bones$variable=="Fem1"]
    } else if (length(Fem2)>0){
      bone <- append(bone, "Fem2.corr")
      n_measures <- n_measures + df_bones$n[df_bones$variable=="Fem2"]
    }
    if (length(Tib1b)>0) {
      bone <- append(bone, "Tib1b")
      n_measures <- n_measures + df_bones$n[df_bones$variable=="Tib1b"]
    } else if (length(Tib1a)>0){
      bone <- append(bone, "Tib1a.corr")
      n_measures <- n_measures + df_bones$n[df_bones$variable=="Tib1a"]
    }

    # Calculate the different indices for male
    Tib1ba <- Tib1b  
    if(length(Tib1b)==0 & length(Tib1a)>0){
      Tib1ba <- Tib1a - 9.6
    }
    Fem12 <- Fem1
    if (length(Fem1)==0 & length(Fem2)>0){
      Fem12 <- Fem2 + 3.2 
    } 

    measures.m <- c()
    measures.m <- append(measures.m, Fem12 * 1.880 + 813.06)
    measures.m <- append(measures.m, Hum1 * 2.894 + 706.41)
    measures.m <- append(measures.m, Tib1ba * 2.376 + 786.64)
    measures.m <- append(measures.m, Rad1 * 3.271+ 859.25)
    measures.m <- append(measures.m, (Fem12 + Tib1ba) * 1.159 + 712.72)
    measures.m <- append(measures.m, (Fem12 * 1.220) + (Tib1ba * 1.080) + 714.43)
    measures.m <- append(measures.m, (Hum1 + Rad1) * 1.730 + 668.55)
    measures.m <- append(measures.m, (Hum1 * 2.769) + (Rad1 * 0.195) + 697.88)
    measures.m <- append(measures.m, (Hum1 * 1.557) + (Fem12 * 1.030) + 683.97)
    measures.m <- append(measures.m, (Fem12 * 0.913) + (Tib1ba * 0.600) +(Hum1 * 1.225) - (Rad1 * 0.187) + 670.49) 
    
    # Calculate the different indices for female 
    Tib1ba <- Tib1b  
    if(length(Tib1b)==0 & length(Tib1a)>0){
      Tib1ba <- Tib1a - 8.7
    }
    Fem12 <- Fem1
    if (length(Fem1)==0 & length(Fem2)>0){
      Fem12 <- Fem2 + 3.3 
    } 
    
    measures.f <- c()
    measures.f <- append(measures.f,  Fem12 * 1.945 + 728.44)
    measures.f <- append(measures.f, Hum1 * 2.754 + 714.75)
    measures.f <- append(measures.f, Tib1ba * 2.352 + 747.74)
    measures.f <- append(measures.f, Rad1 * 3.343 + 812.24)
    measures.f <- append(measures.f, (Fem12 + Tib1ba) * 1.126 + 691.54)
    measures.f <- append(measures.f, (Fem12 * 1.117) + (Tib1ba * 1.125) + 695.61)
    measures.f <- append(measures.f, (Hum1 + Rad1) * 1.628 + 699.11)
    measures.f <- append(measures.f, (Hum1 * 2.582) + (Rad1 * 0.281) + 705.42)
    measures.f <- append(measures.f, (Hum1 * 1.027) + (Fem12 * 1.339) + 674.35)
    measures.f <- append(measures.f, (Fem12 * 0.782) + (Tib1ba * 1.120) +(Hum1 * 1.059) - (Rad1 * 0.711) + 674.69)
    
    # Calculate the different indices for indet.
    measures.i <-(measures.m + measures.f)/2
    
    # calculate mean of each measures group for statures
    statures <- round(c(mean(measures.m), mean(measures.f), mean(measures.i)), 0)

    # write values into data frame of results
    val_indv$sex[i] <- unique(df_bones$Sex)
    val_indv$stature[i] <- statures[as.integer(unique(df_bones$Sex))]
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
