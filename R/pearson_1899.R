#' Calculate body height indices and mean body height based on Pearson 1899
#' 
#' Based on the available measurements of different bones of different individuals, 
#' the body height indices for males and females are calculated.
#'
#' 
#' @title vercellotti_et_al_2009
#'
#' @description 
#' Based on the available measurements of different bones of 
#' different individuals, the body height indices for  
#' males and females are calculated.
#' Returns a data.frame with: 
#'     * Ind: Individual identifyer (rownames), 
#'     * Sex: 
#'     * Statuar: estimated on the provided sex and bone measures, 
#'     * Bone (measure(s)): bones used for calculation, 
#'     * female (statuar): columns with alternative statuar for three sex classes, 
#'     * male (statuar), 
#'     * indet. (statuar) and
#'     * n_measures: number of bonemeasures included, e.g. (left + right)/2
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

pearson_1899 <- function(df){
  
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
    # Get measure values needed
    Hum1 <- df_knochen$mean.value[df_knochen$variable=="Hum1"]
    Rad1 <- df_knochen$mean.value[df_knochen$variable=="Rad1"]
    Fem1 <- df_knochen$mean.value[df_knochen$variable=="Fem1"]
    Tib1b <- df_knochen$mean.value[df_knochen$variable=="Tib1b"]
    Tib1a <- df_knochen$mean.value[df_knochen$variable=="Tib1a"]

    # document bone measures and number used for calculation 
    bone <- c()
    n_measures <- 0
    if (length(Hum1)>0) {
      bone <- append(bone, "Hum1")
      n_measures <- n_measures + df_knochen$n[df_knochen$variable=="Hum1"]
    }
    if (length(Rad1)>0) {
      bone <- append(bone, "Rad1")
      n_measures <- n_measures + df_knochen$n[df_knochen$variable=="Rad1"]
    }
    if (length(Fem1)>0) {
      bone <- append(bone, "Fem1")
      n_measures <- n_measures + df_knochen$n[df_knochen$variable=="Fem1"]
    }
    if (length(Tib1b)>0) {
      bone <- append(bone, "Tib1b")
      n_measures <- n_measures + df_knochen$n[df_knochen$variable=="Tib1b"]
    } else if (length(Tib1a)>0){
      bone <- append(bone, "Tib1a")
      n_measures <- n_measures + df_knochen$n[df_knochen$variable=="Tib1a"]
    }

    # Calculate the different indices for male
    Tib1ba <- Tib1b  
    if(length(Tib1b)==0 & length(Tib1a)>0){
      Tib1ba <- Tib1a + 9.6
    }
    measures.m <- c()
    measures.m <- append(measures.m, Fem1 * 1.880 + 813.06)
    measures.m <- append(measures.m, Hum1 * 2.894 + 706.41)
    measures.m <- append(measures.m, Tib1ba * 2.376 + 786.64)
    measures.m <- append(measures.m, Rad1 * 3.271+ 859.25)
    measures.m <- append(measures.m, (Fem1 + Tib1ba) * 1.159 + 712.72)
    measures.m <- append(measures.m, (Fem1 * 1.220) + (Tib1ba * 1.080) + 714.43)
    measures.m <- append(measures.m, (Hum1 + Rad1) * 1.730 + 668.55)
    measures.m <- append(measures.m, (Hum1 * 2.769) + (Rad1 * 0.195) + 697.88)
    measures.m <- append(measures.m, (Hum1 * 1.557) + (Fem1 * 1.030) + 683.97)
    measures.m <- append(measures.m, (Fem1 * 0.913) + (Tib1ba * 0.600) +(Hum1 * 1.225) - (Rad1 * 0.187) + 670.49) 
    
    # Calculate the different indices for female 
    Tib1ba <- Tib1b  
    if(length(Tib1b)==0 & length(Tib1a)>0){
      Tib1b <- Tib1a + 8.7
    }
    measures.f <- c()
    measures.f <- append(measures.f,  Fem1 * 1.945 + 728.44)
    measures.f <- append(measures.f, Hum1 * 2.754 + 714.75)
    measures.f <- append(measures.f, Tib1ba * 2.352 + 747.74)
    measures.f <- append(measures.f, Rad1 * 3.343 + 812.24)
    measures.f <- append(measures.f, (Fem1 + Tib1ba) * 1.126 + 691.54)
    measures.f <- append(measures.f, (Fem1 * 1.117) + (Tib1ba * 1.125) + 695.61)
    measures.f <- append(measures.f, (Hum1 + Rad1) * 1.628 + 699.11)
    measures.f <- append(measures.f, (Hum1 * 2.582) + (Rad1 * 0.281) + 705.42)
    measures.f <- append(measures.f, (Hum1 * 1.027) + (Fem1 * 1.339) + 674.35)
    measures.f <- append(measures.f, (Fem1 * 0.782) + (Tib1ba * 1.120) +(Hum1 * 1.059) - (Rad1 * 0.711) + 674.69)
    
    # Calculate the different indices for indet.
    measures.i <-(measures.m + measures.f)/2
    
    # calculate mean of each measures group for statuars
    statuars <- round(c(mean(measures.m), mean(measures.f), mean(measures.i)), 0)

    # write values into data frame of results
    val_indv$sex[i] <- unique(df_knochen$Sex)
    val_indv$statuar[i] <- statuars[as.integer(unique(df_knochen$Sex))]
    val_indv$bone <- paste(bone, collapse = ", ")
    val_indv$female[i] <- statuars[2]
    val_indv$male[i] <- statuars[1]
    val_indv$indet[i] <- statuars[3]
    val_indv$n_measures[i] <- n_measures
  }
  
  if (dim(val_indv)[1] == 0) {
    print("There is no usable bone measurement / indice available for the chosen formula")
  }
  
  return(val_indv)
}
