#' Calculate body height indices and mean body height based on Olivier et al 1978.
#' 
#' Based on the available measurements of different bones of different individuals, the body height indices for males and females are calculated according to Vercelotti et al. 2009.
#'
#' 
#' @title olivier_etal_1978
#'
#' @description 
#' Based on the available measurements of different bones of 
#' different individuals, the body height indices for  males and females are 
#' calculated according to Olivier et al (1978). 
#' The regression formula are hierarchical from combinations of different 
#' bone measures to single bone measures. For the multiple regressions the average 
#' of left and right bone is used, allthough "asymmetry to be virtually negligible: 
#' it is only significant for the right ulna" (Olivier et al 1978, 515). 
#' For individual bone measures regression formula a given for left an right side
#' in the case of male individuals. Wheras regression formula for females are 
#' presented only for left bones, likely due to the data based on.
#' In  consequence for male individuals statuar estimation on a single bone is
#' calculated 1. upon the hierarchy given by the table (r, s.d.) and 2. on the mean of 
#' left an right bone or 3. on one of both sides.
#' Wheras for female individuals statuar estimation based on one bone considers 
#' 1. the hierarchy given by the table (r and s.d) using the mean of left and/or 
#' right side.
#' For male 15 multiple regressions are provided, for female only 9, 
#' in addition using different bone measures within each hierarchy. 
#' In consequence, stature estimation for both sexes do not correlate and 
#' the alternative stature will not be calculated.
#' Only the first applicable measure of the given hierarchy will be used.
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

#######################################################
calc.stature.m <- function (df_knochen){
  
  # get all optional needed measures
  Hum1 <- df_knochen$mean.value[df_knochen$variable=="Hum1"]
  Rad1b <- df_knochen$mean.value[df_knochen$variable=="Rad1b"]
  Uln1 <- df_knochen$mean.value[df_knochen$variable=="Uln1"]
  Fem2 <- df_knochen$mean.value[df_knochen$variable=="Fem2"]
  Tib1b <- df_knochen$mean.value[df_knochen$variable=="Tib1b"]
  Fib1 <- df_knochen$mean.value[df_knochen$variable=="Fib1"]
  
  stature.m<-c()
  
  # Long list of multiple regression if measures > 1, else if <=1 
  
  if (length(c(Hum1, Rad1b, Uln1, Fem2, Tib1b, Fib1))>1) {
    
    # if there is more than one measure do multiple regression
    
    stature.m <- (Hum1 * 0.593) + (Fem2 * 0.983) + (Fib1 * 1.384) + 539.0
    indice <- "Hum1&Fem2&Fib1"
    n_measures <- df_knochen$n[df_knochen$variable=="Hum1"] + 
      df_knochen$n[df_knochen$variable=="Fem2"] +
      df_knochen$n[df_knochen$variable=="Fib1"]
    if (length(stature.m)==0){
      stature.m <- (Fem2 * 1.213) + (Fib1 * 1.548) + 569.3
      indice <- "Fem2&Fib1"
      n_measures <- df_knochen$n[df_knochen$variable=="Fem2"] +
        df_knochen$n[df_knochen$variable=="Fib1"]
    } 
    if (length(stature.m)==0){
      stature.m <- (Hum1 * 0.717) + (Fem2 * 1.012) + (Tib1b * 1.215) + 536.3
      indice <- "Hum1&Fem2&Tib1b"
      n_measures <- df_knochen$n[df_knochen$variable=="Hum1"] + 
        df_knochen$n[df_knochen$variable=="Fem2"] +
        df_knochen$n[df_knochen$variable=="Tib1b"]
    } 
    if (length(stature.m)==0){
      stature.m <- (Fem2 * 1.307) + (Tib1b * 1.388) + 573.4
      indice <- "Fem2&Tib1b"
      n_measures <- df_knochen$n[df_knochen$variable=="Fem2"] +
        df_knochen$n[df_knochen$variable=="Tib1b"]
    } 
    if (length(stature.m)==0){
      stature.m <- (Hum1 * 1.273) + (Tib1b * 1.820) + 589.4
      indice <- "Hum1&Tib1b"
      n_measures <- df_knochen$n[df_knochen$variable=="Hum1"] + 
        df_knochen$n[df_knochen$variable=="Tib1b"]
    } 
    if (length(stature.m)==0){
      stature.m <- (Hum1 * 1.148) + (Fib1 * 1.966) + 592.8
      indice <- "Hum1&Fib1"
      n_measures <- df_knochen$n[df_knochen$variable=="Hum1"] + 
        df_knochen$n[df_knochen$variable=="Fib1"]
    } 
    if (length(stature.m)==0){
      stature.m <- (Rad1b * 1.562) + (Fem2 * 1.776) + 499.0
      indice <- "Rad1b&Fem2"
      n_measures <- df_knochen$n[df_knochen$variable=="Rad1b"] + 
        df_knochen$n[df_knochen$variable=="Fem2"] 
    } 
    if (length(stature.m)==0){
      stature.m <- (Rad1b * 0.874) + (Fib1 * 2.271) + 648.4
      indice <- "Rad1b&Fib1"
      n_measures <- df_knochen$n[df_knochen$variable=="Rad1b"] + 
        df_knochen$n[df_knochen$variable=="Fib1"]
    } 
    if (length(stature.m)==0){
      stature.m <- (Uln1 * 1.234) + (Fem2 * 1.935) + 484.1
      indice <- "Uln1&Fem2"
      n_measures <- df_knochen$n[df_knochen$variable=="Uln1"] + 
        df_knochen$n[df_knochen$variable=="Fem2"]
    } 
    if (length(stature.m)==0){
      stature.m <- (Hum1 * 1.121) + (Fem2 * 1.760) + 515.6
      indice <- "Hum1&Fem2"
      n_measures <- df_knochen$n[df_knochen$variable=="Hum1"] + 
        df_knochen$n[df_knochen$variable=="Fem2"]
    } 
    if (length(stature.m)==0){
      stature.m <- (Uln1 * 0.444) + (Fib1 * 2.492) + 664.3
      indice <- "Uln1&Fib1"
      n_measures <- df_knochen$n[df_knochen$variable=="Uln1"] + 
        df_knochen$n[df_knochen$variable=="Fib1"]
    } 
    if (length(stature.m)==0){
      stature.m <- (Rad1b * 1.189) + (Tib1b * 2.025) + 637.8
      indice <- "Rad1b&Tib1b"
      n_measures <- df_knochen$n[df_knochen$variable=="Rad1b"] +
        df_knochen$n[df_knochen$variable=="Tib1b"]
    } 
    if (length(stature.m)==0){
      stature.m <- (Uln1 * 0.789) + (Tib1b * 2.248) + 644.7
      indice <- "Uln1&Tib1b"
      n_measures <- df_knochen$n[df_knochen$variable=="Uln1"] + 
        df_knochen$n[df_knochen$variable=="Tib1b"]
    } 
    if (length(stature.m)==0){
      stature.m <- (Hum1 * 1.893) + (Rad1b * 2.163) + 541.2
      indice <- "Hum1&Rad1b"
      n_measures <- df_knochen$n[df_knochen$variable=="Hum1"] + 
        df_knochen$n[df_knochen$variable=="Rad1b"] 
    } 
    if (length(stature.m)==0){
      stature.m <- (Hum1 * 2.257) + (Uln1 * 1.586) + 532.9
      indice <- "Hum1&Uln1"
      n_measures <- df_knochen$n[df_knochen$variable=="Hum1"] + 
        df_knochen$n[df_knochen$variable=="Uln1"]
    }
  } else {
    # there is only one or no measure per individual
    if ((length(stature.m)==0) & (length(Fib1)>0)) {
      n_measures <- df_knochen$n[df_knochen$variable=="Fib1"]
      if (n_measures==2){
        stature.m <- (Fib1 * 2.6700) + 715.30
        indice <-"Fib1.rl"
      } else if (df_knochen$right[df_knochen$variable=="Fib1"]==TRUE){
        stature.m <- (Fib1 * 2.6559) + 721.0
        indice <-"Fib1.r"
      } else {
        stature.m <- (Fib1l * 2.6841) + 709.6
        indice <-"Fib1.l"
      }
    }
    if ((length(stature.m)==0) & (length(Tib1b)>0)){
      n_measures <- df_knochen$n[df_knochen$variable=="Tib1b"]
      if (n_measures==2){
        stature.m <- (Tib1b * 2.6061) + 716.90
        indice <- "Tib1b.rl"
      } else if (df_knochen$right[df_knochen$variable=="Tib1b"]==TRUE){
        stature.m <- (Tib1br * 2.6202) + 713.2
        indice <- "Tib1b.r"
      } else {
        stature.m <- (Tib1bl * 2.5919) + 720.6
        indice <- "Tib1b.l"
      }
    }
    if ((length(stature.m)==0) & (length(Fem2)>0)){
      n_measures <- df_knochen$n[df_knochen$variable=="Fem2"]
      if (n_measures==2){
        stature.m <- (Fem2 * 2.4184) + 585.05
        indice <- "Fem2.rl"
      } else if (df_knochen$right[df_knochen$variable=="Fem2"]==TRUE){
        stature.m <- (Fem2r * 2.4165) + 586.8
        indice <- "Fem2.r"
      } else {
        stature.m <- (Fem2l * 2.4202) + 583.3
        indice <- "Fem2.l"
      }
    }
    if ((length(stature.m)==0) & (length(Hum1)>0)){
      n_measures <- df_knochen$n[df_knochen$variable=="Hum1"]
      if (n_measures==2){
        stature.m <- (Hum1 * 3.1735) + 644.15
        indice <- "Hum1.rl"
      } else if (df_knochen$right[df_knochen$variable=="Hum1"]==TRUE){
        stature.m <- (Hum1r * 3.1564) + 646.4
        indice <- "Hum1.r"
      } else {
        stature.m <- (Hum1l * 3.1906) + 641.9
        indice <- "Hum1.l"
      }
    }
    if ((length(stature.m)==0) & (length(Rad1b)>0)){
      n_measures <- df_knochen$n[df_knochen$variable=="Rad1b"]
      if (n_measures==2){
        stature.m <- (Rad1b * 4.2323) + 664.90
        indice <- "Rad1b.rl"
      } else if (df_knochen$right[df_knochen$variable=="Rad1b"]==TRUE){
        stature.m <- (Rad1br * 4.2865) + 648.5
        indice <- "Rad1b.r"
      } else {
        stature.m <- (Rad1bl * 4.1780) + 681.3
        indice <- "Rad1b.l"
      }
    }
    if ((length(stature.m)==0) & (length(Uln1)>0)){
      n_measures <- df_knochen$n[df_knochen$variable=="Uln1"]
      if (n_measures==2){
        stature.m <- (Uln1* 3.9119) + 674.75
        indice <- "Uln1.rl"
      } else if (df_knochen$right[df_knochen$variable=="Uln1"]==TRUE){
        stature.m <- (Uln1l * 3.9582) + 667.1
        indice <- "Uln1.r"
      } else {
        stature.m <- (Uln1r * 3.8656) + 682.4
        indice <- "Uln1.l"
      }
    } 
  } # End of else for 1 measure
  # End of male stature estimation
  
  return(list("stature"=stature.m, "indice"=indice, "n_measures"=n_measures))
  
} # End of Function calc.stature.m

##############################################################

calc.stature.f <- function (df_knochen){
  
  # get all optional needed measures
  Hum1 <- df_knochen$mean.value[df_knochen$variable=="Hum1"]
  Rad1b <- df_knochen$mean.value[df_knochen$variable=="Rad1b"]
  Uln1 <- df_knochen$mean.value[df_knochen$variable=="Uln1"]
  Fem2 <- df_knochen$mean.value[df_knochen$variable=="Fem2"]
  Tib1b <- df_knochen$mean.value[df_knochen$variable=="Tib1b"]
  Fib1 <- df_knochen$mean.value[df_knochen$variable=="Fib1"]
  
  # check for different combinations of measures for female stature estimation
  stature.f<-c()
  
  # Long list of multiple regression if measures > 1, else if <=1 
  
  if (length(c(Hum1, Rad1b, Uln1, Fem2, Tib1b, Fib1))>1) {
    
    # if there is more than one measure do multiple regression
    
    stature.f <- (Hum1 * 0.771) + (Fem2 * 0.934) + (Tib1b * 1.114) + 565.4
    indice.f <- "Hum1&Fem2&Tib1b"
    n_measures.f <- df_knochen$n[df_knochen$variable=="Hum1"] + 
      df_knochen$n[df_knochen$variable=="Fem2"] +
      df_knochen$n[df_knochen$variable=="Tib1b"]
    if (length(stature.f)==0){
      stature.f <- (Fem2 * 1.513) + (Tib1b * 1.265) + 513.3
      indice.f <- "Fem2&Tib1b"
      n_measures.f <- df_knochen$n[df_knochen$variable=="Fem2"] +
        df_knochen$n[df_knochen$variable=="Tib1b"]
    } 
    if (length(stature.f)==0){
      stature.f <- (Uln1 * 2.468) + (Fem2 * 1.236) + 484.1
      indice.f <- "Uln1&Fem2"
      n_measures.f <- df_knochen$n[df_knochen$variable=="Uln1"] + 
        df_knochen$n[df_knochen$variable=="Fem2"] 
    } 
    if (length(stature.f)==0){
      stature.f <- (Rad1b * 2.408) + (Fem2 * 1.258) + 558.0
      indice.f <- "Rad1b&Fem2"
      n_measures.f <- df_knochen$n[df_knochen$variable=="Rad1b"] +
        df_knochen$n[df_knochen$variable=="Fem2"]
    } 
    if (length(stature.f)==0){
      stature.f <- (Hum1 * 1.661) + (Fem2 * 1.240) + 543.3
      indice.f <- "Hum1&Fem2"
      n_measures.f <- df_knochen$n[df_knochen$variable=="Hum1"] + 
        df_knochen$n[df_knochen$variable=="Fem2"]
    } 
    if (length(stature.f)==0){
      stature.f <- (Hum1 * 1.549) + (Uln1 * 2.408) + 532.9
      indice.f <- "Hum1&Uln1"
      n_measures.f <- df_knochen$n[df_knochen$variable=="Hum1"] + 
        df_knochen$n[df_knochen$variable=="Uln1"]
    } 
    if (length(stature.f)==0){
      stature.f <- (Hum1 * 1.605) + (Rad1b * 2.316) + 609.4
      indice.f <- "Hum1&Rad1b"
      n_measures.f <- df_knochen$n[df_knochen$variable=="Hum1"] + 
        df_knochen$n[df_knochen$variable=="Rad1b"] 
    } 
    if (length(stature.f)==0){
      stature.f <- (Rad1b * 2.841) + (Tib1b * 0.871) + 705.4
      indice.f <- "Rad1b&Tib1b"
      n_measures.f <- df_knochen$n[df_knochen$variable=="Rad1b"] + 
        df_knochen$n[df_knochen$variable=="Tib1b"]
    } 
    if (length(stature.f)==0){
      stature.f <- (Uln1 * 2.910) + (Tib1b * 0.838) + 625.3
      indice.f <- "Uln1&Tib1b"
      n_measures.f <- df_knochen$n[df_knochen$variable=="Uln1"] + 
        df_knochen$n[df_knochen$variable=="Tib1b"]
    } 
  } else {
    # there is only one or no measure per individual
    if ((length(stature.f)==0) & (length(Tib1b)>0)){
      stature.f <- (Tib1b * 2.3000) + 804.0
      n_measures.f <- df_knochen$n[df_knochen$variable=="Tib1b"]
      if (n_measures.f==2){
        indice.f <- "Tib1b.rl"
      } else if (df_knochen$right[df_knochen$variable=="Tib1b"]==TRUE){
        indice.f <- "Tib1b.r"
      } else {
        indice.f <- "Tib1b.l"
      }
    }
    if ((length(stature.f)==0) & (length(Fem2)>0)){
      stature.f <- (Fem2 * 2.0960) + 702.0
      n_measures.f <- df_knochen$n[df_knochen$variable=="Fem2"]
      if (n_measures.f==2){
        indice.f <- "Fem2.rl"
      } else if (df_knochen$right[df_knochen$variable=="Fem2"]==TRUE){
        indice.f <- "Fem2.r"
      } else {
        indice.f <- "Fem2.l"
      }
    }
    if ((length(stature.f)==0) & (length(Hum1)>0)){
      stature.f <- (Hum1 * 3.0882) + 623.1
      n_measures.f <- df_knochen$n[df_knochen$variable=="Hum1"]
      if (n_measures.f==2){
        indice.f <- "Hum1.rl"
      } else if (df_knochen$right[df_knochen$variable=="Hum1"]==TRUE){
        indice.f <- "Hum1.r"
      } else {
        indice.f <- "Hum1.l"
      }
    }
    if ((length(stature.f)==0) & (length(Rad1b)>0)){
      n_measures.f <- df_knochen$n[df_knochen$variable=="Rad1b"]
      stature.f <- (Rad1b * 4.1337) + 703.0
      if (n_measures.f==2){
        indice.f <- "Rad1b.rl"
      } else if (df_knochen$right[df_knochen$variable=="Rad1b"]==TRUE){
        indice.f <- "Rad1b.r"
      } else {
        indice.f <- "Rad1b.l"
      }
    }
    if ((length(stature.f)==0) & (length(Uln1)>0)){
      n_measures.f <- df_knochen$n[df_knochen$variable=="Uln1"]
      stature.f <- (Uln1 * 4.0931) + 637.1
      if (n_measures.f==2){
        indice.f <- "Uln1.rl"
      } else if (df_knochen$right[df_knochen$variable=="Uln1"]==TRUE){
        indice.f <- "Uln1.r"
      } else {
        indice.f <- "Uln1.l"
      }
    } 
  } # End of else for 1 measure
  # End of female stature estimation

  return(list("stature"=stature.f, "indice"=indice.f, "n_measures"=n_measures.f))

} # End of Function calc.stature.f


###################################
olivier_etal_1978 <- function(df){
  # stupid logic, but:
  # - laterality is not needed for the multiple regression or women
  # - laterality only needed for men with only one bone. 
  # - What to do if laterality of 2 measures is unknown?

  # create variable side for laterality and delete corresponding info from measure
  # n=2: both measures used, n=1 & right=T: right side, n=1 & right=F: left side 
  df$side<-rep("l",nrow(df))
  df$side[grepl(".r", df$variable)]<-"r"
  df$variable<-gsub("([rl]$)","", df$variable) #
  # aggregate values for each measure and individual with right==true in case of
  df %>%  
    group_by(Ind, Sex, Group, variable) %>% 
    summarise(mean.value = mean(value), n = n(), right = any(side=="r")) %>%
    as.data.frame()-> df
  
  
  vec_indv <- unique(df$Ind) # extract names and quantity of unique individuals

  # Initialize data frame for later storage of different mean body heights
  val_indv<- as.data.frame(matrix(ncol=7, nrow=length(vec_indv)), row.names=vec_indv)
  colnames(val_indv) <-c("sex", "stature", "bone", "female", "male", "indet", "n_measures")
  val_indv$sex <- factor(val_indv$sex, levels = c("m", "f", "i"))


  # check available values for different variables needed for 
  for (i in 1:length(vec_indv)){
    df_knochen <- subset(df, subset = Ind==vec_indv[i])
    
    stature.m <- calc.stature.m(df_knochen)
    stature.f <- calc.stature.f(df_knochen)

    #  mean for indet. sex,  all bone labels and sum of n_measures 
    
    stature.i <- mean(c(stature.m[[1]], stature.f[[1]]))
    indice.i <- paste(stature.m[[2]], stature.f[[2]], sep = ", ")
    n_measures.i <- stature.m[[3]] + stature.m[[3]]
    
    # vectors for stature estimations, indices and n_measures
    statures <- c(stature.m[[1]], stature.f[[1]], stature.i)
    statures <- round(statures, 0)
    indices <- c(stature.m[[2]], stature.f[[2]], indice.i)
    n_measures <- c(stature.m[[3]], stature.f[[3]], n_measures.i)
    
    # write values into data frame of results
    val_indv$sex[i] <- unique(df_knochen$Sex)
    val_indv$stature[i] <- statures[as.integer(unique(df_knochen$Sex))]
    val_indv$bone[i] <- indices[as.integer(unique(df_knochen$Sex))]
    val_indv$female[i] <- statures[2]
    val_indv$male[i] <- statures[1]
    val_indv$indet[i] <- statures[3]
    val_indv$n_measures[i] <- n_measures[as.integer(unique(df_knochen$Sex))]
  } # next individual
  
  if (dim(val_indv)[1] == 0) {
    print("There is no usable bone measurement / indice available for the chosen formula")
  }
  
  #rm(i, vec_indv)

  return(val_indv)
}