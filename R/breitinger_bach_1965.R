#' Calculate stature estimation according to:
#' Breitinger 1938 & Bach 1965.
#' 
#' @title breitinger_bach_1965
#'
#' @description 
#' Stature estimation (mm) based on the mean of different regression calculations,
#' separated  by sex (Breitinger 1938, Bach 1965).
#' Bone measures used: Hum2, Hum1, Rad1b, Fem1, Tib1b
#' 
#' If bone measures for left and right are provided the mean value will be used,
#' but for statistic information 2 bones will be counted (n_measures).
#' If sex is indet. the mean of male and female stature estimation is given.
#' Breitinger (1938) does not show the regression equation for Hum1, but the 
#' equation can be derived from the values given in the table p. 272 
#' (see Siegmund 2010, p. 112). In contrast, Bach (1965) gives both equations.
#'  In addition, Bach (1965, 20) states to use only one of the Humeri measures 
#'  for estimation (Hum1 or Hum2 or the mean of both) to avoid inadmissable 
#'  multiplication of the bone within the stature estimation. 
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
#' @param df data.frame, containing informations on individual, bone and measurement
#'  
#' @return data.frame, containing one data.frame with all calculated indices for every individual
#'           
#' @author Christoph Rinne <\email{crinne@@ufg.uni-kiel.de}>
#' 
#' @examples
#' 
#'@export

library(dplyr)

breitinger_bach_1965 <- function(df){
  
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
  
  # check available values for different variables needed for 
  for (i in 1:length(vec_indv)){
    df_bones <- subset(df, subset=df$Ind == vec_indv[i])
    # Get measure values needed
    Hum2 <- df_bones$mean.value[df_bones$variable=="Hum2"]
    Hum1 <- df_bones$mean.value[df_bones$variable=="Hum1"]
    Rad1b <- df_bones$mean.value[df_bones$variable=="Rad1b"]
    Fem1 <- df_bones$mean.value[df_bones$variable=="Fem1"]
    Tib1b <- df_bones$mean.value[df_bones$variable=="Tib1b"]

    # document bone measures and number used for calculation
    
    bone <- c()
    n_measures <- 0
    if (length(Hum2)>0 & length(Hum1)>0)  {
      bone <- append(bone, "Hum2&1")
      n_measures <- n_measures +
      df_bones$n[df_bones$variable=="Hum2"] +
      df_bones$n[df_bones$variable=="Hum1"]
    } else if (length(Hum2)>0) {
      bone <- append(bone, "Hum2")
      n_measures <- n_measures + df_bones$n[df_bones$variable=="Hum2"]
    } else if (length(Hum1)>0) {
      bone <- append(bone, "Hum1")
      n_measures <- n_measures + df_bones$n[df_bones$variable=="Hum1"]
    }
    if (length(Rad1b)>0) {
      bone <- append(bone, "Rad1b")
      n_measures <- n_measures + df_bones$n[df_bones$variable=="Rad1b"]
    }
    if (length(Fem1)>0) {
      bone <- append(bone, "Fem1")
      n_measures <- n_measures + df_bones$n[df_bones$variable=="Fem1"]
    }
    if (length(Tib1b)>0) {
      bone <- append(bone, "Tib1b")
      n_measures <- n_measures + df_bones$n[df_bones$variable=="Tib1b"]
    } 

    # Calculate the different indices for male
    measures.m <- c()
    measures.m <- append (measures.m, mean(c((Hum1 / 10) * 2.71 + 81.33), ((Hum2 / 10) * 2.715 + 83.21)))
    measures.m <- append (measures.m, (Rad1b / 10) * 2.968 + 97.09)
    measures.m <- append (measures.m, (Fem1 / 10) * 1.645 + 94.31)
    measures.m <- append (measures.m, (Tib1b / 10) * 1.988 + 95.59)
    
    # Calculate the different indices for female 
    measures.f <- c()
    measures.f <- append (measures.f, mean(c((Hum1 / 10) * 2.121 + 98.38), ((Hum2 / 10) * 2.121 + 99.44))) 
    measures.f <- append (measures.f, (Rad1b / 10) * 1.925 + 116.89)
    measures.f <- append (measures.f, (Fem1 / 10) * 1.313 + 106.69)
    measures.f <- append (measures.f, (Tib1b / 10) * 1.745 + 95.91)
    
    # Calculate the different indices for indet.
    measures.i <-(measures.m + measures.f)/2
    
    # calculate mean of each measures group for statures
    # as the regression equations are calculated in cm the result is converted to mm

    statures <- round(10 * c(mean(measures.m), mean(measures.f), mean(measures.i)), 0)

    # write values into data frame of results
    val_indv$sex[i] <- unique(df_bones$Sex)
    val_indv$stature[i] <- statures[as.integer(unique(df_bones$Sex))]
    val_indv$bone[i] <- paste(bone, collapse = ", ")
    val_indv$group[i] <- unique(df_bones$Group)
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
