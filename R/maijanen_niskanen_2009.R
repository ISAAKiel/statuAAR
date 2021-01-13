#' Calculate stature estimation according to:
#' Maijanen, Niskanen 2009
#' 
#' @title maijanen_niskanen_2009
#'
#' @description 
#' Stature estimation (mm) based on the hierarchy of different regression calculations,
#' separated  by sex (Maijanen, Niskanen 2009).
#' Bone measures used: Fem2+Tib1, Fem1+Tib1, Fem2, Fem1, Tib1, Hum1, Rad1, Fib1, Uln1
#' 
#' If bone measures for left and right are provided the mean value will be used,
#' but for statistic information 2 bones will be counted (n_measures).
#' Maijanen & Niskanen propose propose to use first the combination of femur and 
#' tibia, followed by calculations on the hierarchie of single measures. 
#' Thea present regression calculations for the combination of male and female
#' individuals recomended in case of undetermined sex (indet.).
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

maijanen_niskanen_2009 <- function(df){
  
  df$variable<-gsub("([rl]$)","", df$variable) # laterality not needed
  # aggregate values for each measure and individual
  df %>%  
    group_by(Ind, Sex, Group, variable) %>% 
    summarise(mean.value = mean(value), n = n()) -> df
  
  vec_indv <- unique(df$Ind) # extract names and quantity of unique individuals
  
  # Initialize data frame for later storage of different mean body heights
  val_indv <- as.data.frame(matrix(ncol=8, nrow=length(vec_indv)), row.names=vec_indv)
  colnames(val_indv) <-c("sex", "stature", "bone", "group", "female", "male", "indet", "n_measures")
  val_indv$sex <- factor(val_indv$sex, labels = c("m", "f", "indet"), levels = c(1,2,3))
  
  # Calculte in hierarchical order
  
  # check available values for different variables needed for 
  for (i in 1:length(vec_indv)){
    df_bones <- subset(df, subset=df$Ind == vec_indv[i])
    
    # get all optional needed measures
    # get all optional needed measures
    Fem1 <- df_bones$mean.value[df_bones$variable=="Fem1"]
    Fem2 <- df_bones$mean.value[df_bones$variable=="Fem2"]
    Tib1 <- df_bones$mean.value[df_bones$variable=="Tib1"]
    Hum1 <- df_bones$mean.value[df_bones$variable=="Hum1"]
    Rad1 <- df_bones$mean.value[df_bones$variable=="Rad1"]
    Fib1 <- df_bones$mean.value[df_bones$variable=="Fib1"]
    Uln1 <- df_bones$mean.value[df_bones$variable=="Uln1"]
    
    # check for different combinations of measures
    # Fem2 & Tib1
    if (length(Fem2)>0 & length(Tib1)>0){
      stature.m <- ((Fem2 + Tib1) * 1.62) + 353.3
      stature.f <- ((Fem2 + Tib1) * 1.53) + 416.3
      stature.i <- ((Fem2 + Tib1) * 1.64) + 338.2
      statures <- c(stature.m, stature.f, stature.i)
      indice <- "1. Fem2+Tib1b"
      n_measures <- df_bones$n[df_bones$variable=="Fem2"] + 
        df_bones$n[df_bones$variable=="Tib1b"]
    } else if (length(Fem1)>0 & length(Tib1)>0){
      stature.m <- ((Fem1 + Tib1) * 1.63) + 341.3
      stature.f <- ((Fem1 + Tib1) * 1.49) + 442.7
      stature.i <- ((Fem1 + Tib1) * 1.64) + 330.0
      statures <- c(stature.m, stature.f, stature.i)
      indice <- "2. Fem1+Tib1"
      n_measures <- df_bones$n[df_bones$variable=="Fem1"] + 
        df_bones$n[df_bones$variable=="Tib1"]
      # Fem2
      } else if (length(Fem2)>0) {
      stature.m <- (Fem2 * 2.93) + 350.1
      stature.f <- (Fem2 * 2.94) + 343.0
      stature.i <- (Fem2 * 2.94) + 342.3
      statures <- c(stature.m, stature.f, stature.i)
      indice <- "3. Fem2"
      n_measures <- df_bones$n[df_bones$variable=="Fem2"]
      # Fem1
    } else if (length(Fem1)>0) {
      stature.m <- (Fem1 * 2.96) + 325.5
      stature.f <- (Fem1 * 2.79) + 396.3
      stature.i <- (Fem1 * 2.95) + 327.3
      statures <- c(stature.m, stature.f, stature.i)
      indice <- "4. Fem1"
      n_measures <- df_bones$n[df_bones$variable=="Fem1"]
      # Fib1
    } else if (length(Fib1)>0) {
      stature.m <- (Fib1 * 3.61) + 381.4
      stature.f <- (Fib1 * 3.19) + 518.8
      stature.i <- (Fib1 * 3.60) + 383.8
      statures <- c(stature.m, stature.f, stature.i)
      indice <- "5. Fib1"
      n_measures <- df_bones$n[df_bones$variable=="Fib1"]
      # Tib1
    } else if (length(Tib1)>0) {
      stature.m <- (Tib1 * 3.46) + 427.7
      stature.f <- (Tib1 * 2.98) + 572.6
      stature.i <- (Tib1 * 3.57) + 380.5
      statures <- c(stature.m, stature.f, stature.i)
      indice <- "6. Tib1"
      n_measures <- df_bones$n[df_bones$variable=="Tib1"]
      # Hum1
    } else if (length(Hum1)>0) {
      stature.m <- (Hum1 * 4.06) + 327.4
      stature.f <- (Hum1 * 3.95) + 348.2
      stature.i <- (Hum1 * 4.29) + 246.8
      statures <- c(stature.m, stature.f, mean(c(stature.m, stature.f)))
      indice <- "7. Hum1"
      n_measures <- df_bones$n[df_bones$variable=="Hum1"]
      # Rad1
    } else if (length(Rad1)>0) {
      stature.m <- (Rad1 * 5.94) + 198.6
      stature.f <- (Rad1 * 6.10) + 174.1
      stature.i <- (Rad1 * 5.72) + 259.9 
      statures <- c(stature.m, stature.f, stature.i)
      indice <- "8. Rad1"
      n_measures <- df_bones$n[df_bones$variable=="Rad1"]
      # Uln1
    } else if (length(Uln1)>0) {
      stature.m <- (Uln1 * 5.88) + 90.5
      stature.f <- (Uln1 * 5.92) + 95.9
      stature.i <- (Uln1 * 5.52) + 194.2 
      statures <- c(stature.m, stature.f, stature.i)
      indice <- "8. Uln1"
      n_measures <- df_bones$n[df_bones$variable=="Uln1"]
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
