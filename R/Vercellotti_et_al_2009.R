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
#'     * female (stature): columns with alternative stature for three sex classes, 
#'     * male (stature), 
#'     * indet. (stature)
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

vercellotti_et_al_2009 <- function(df){
  
  df$variable<-gsub("([rl]$)","", df$variable) # laterality not needed
  # aggregate values for each measure an Individual
  options(dplyr.summarise.inform = FALSE)
  df %>%  
    group_by(Ind, Sex, Group, variable) %>% 
    summarise(mean.value = mean(value), n = n()) -> df
  
  vec_indv <- unique(df$Ind) # extract names and quantity of unique individuals
  
  # Initialize data frame for later storage of different mean body heights
  val_indv <- as.data.frame(matrix(ncol=8, nrow=length(vec_indv)), row.names=vec_indv)
  colnames(val_indv) <-c("sex", "stature", "indice", "female", "male", "indet", "n_measures")
  
  # check available values for different variables needed for 
  for (i in 1:length(vec_indv)){
    df_bones <- subset(df, subset=df$Ind == vec_indv[i])
    
    # Fem2 
    if ("Fem2" %in% df_bones$variable){
      Fem2 <- df_bones$value[df_bones$variable == "Fem2"]
    } else if (("Fem2r" %in% df_bones$variable) & !("Fem2l" %in% df_bones$variable)){
      Fem2 <- df_bones$value[df_bones$variable == "Fem2r"]
    } else if (("Fem2l" %in% df_bones$variable) & !("Fem2r" %in% df_bones$variable)){
      Fem2 <- df_bones$value[df_bones$variable == "Fem2l"]
    } else if (("Fem2l" %in% df_bones$variable) & ("Fem2r" %in% df_bones$variable)){
      Fem2 <- (((df_bones$value[df_bones$variable == "Fem2l"])+(df_bones$value[df_bones$variable == "Fem2r"]))/2)
    }
    
    # Tib1 
    if ("Tib1" %in% df_bones$variable){
      Tib1 <- df_bones$value[df_bones$variable == "Tib1"]
    } else if (("Tib1r" %in% df_bones$variable) & !("Tib1l" %in% df_bones$variable)){
      Tib1 <- df_bones$value[df_bones$variable == "Tib1r"]
    } else if (("Tib1l" %in% df_bones$variable) & !("Tib1r" %in% df_bones$variable)){
      Tib1 <- df_bones$value[df_bones$variable == "Tib1l"]
    } else if (("Tib1l" %in% df_bones$variable) & ("Tib1r" %in% df_bones$variable)){
      Tib1 <- (((df_bones$value[df_bones$variable == "Tib1l"])+(df_bones$value[df_bones$variable == "Tib1r"]))/2)
    }  
    
    # Check if values Fem2 and/or Tib1 were present and if so insert them in respective function
    
    if (exists("Fem2") & exists("Tib1")){
      K_vercellotti_2009_m <- ((Fem2+Tib1) * 1.50) + 469
      K_vercellotti_2009_f <- ((Fem2+Tib1) * 1.55) + 390
      rm(Tib1, Fem2)
      # Store results in data frame for every individual
      if (df_bones$Sex == "m"){ 
      val_indv$male[i] <- K_vercellotti_2009_m
    } else if (df_bones$Sex == "f"){
      val_indv$female[i] <- K_vercellotti_2009_f
    } else {val_indv$indet[i] <- (K_vercellotti_2009_f+K_vercellotti_2009_m)/2}
      val_indv$indice[i] <- "Fem2/Tib1"
      next
    } else if (exists("Fem2")){
      if (df_bones$Sex == "m"){ 
        val_indv$male[i] <- (Fem2 * 2.70) + 481
      } else if (df_bones$Sex == "f"){
        val_indv$female[i] <- (Fem2 * 2.89) + 365
      } else {val_indv$indet[i] <- (((Fem2 * 2.70) + 481)+((Fem2 * 2.89) + 365))/2}
      val_indv$indice[i] <- "Fem2"
      rm(Fem2)
      next
    }
    
    # Fem1 
    if ("Fem1" %in% df_bones$variable){
      Fem1 <- df_bones$value[df_bones$variable == "Fem1"]
    } else if (("Fem1r" %in% df_bones$variable) & !("Fem1l" %in% df_bones$variable)){
      Fem1 <- df_bones$value[df_bones$variable == "Fem1r"]
    } else if (("Fem1l" %in% df_bones$variable) & !("Fem1r" %in% df_bones$variable)){
      Fem1 <- df_bones$value[df_bones$variable == "Fem1l"]
    } else if (("Fem1l" %in% df_bones$variable) & ("Fem1r" %in% df_bones$variable)){
      Fem1 <- (((df_bones$value[df_bones$variable == "Fem1l"])+(df_bones$value[df_bones$variable == "Fem1r"]))/2)
    }
    
    # Check if value Fem1 was present and if so insert it in respective function
#    if (exists("Fem1")){
#      K_vercellotti_2009_m <- (Fem1 * 2.61) + 515
#      K_vercellotti_2009_f <- (Fem1 * 2.89) + 353
#      rm(Fem1)
#      # Store results in data frame for every individual
#      val_indv$male[i] <- K_vercellotti_2009_m
#      val_indv$female[i] <- K_vercellotti_2009_f
#      val_indv$indice[i] <- "Fem1"
#      next
#    } 
    # Check if value Fem1 is present and if so calculate and write data
    if (exists("Fem1")){
      val_indv$female[i] <- (Fem1 * 2.89) + 353
      val_indv$male[i] <- (Fem1 * 2.61) + 515
      val_indv$indet[i] <- mean(c(((Fem1 * 2.61) + 515), ((Fem1 * 2.89) + 353)))
      val_indv$sex[i] <- vec_indv$Sex # get Sex from the vector of individual data!
      if (val_indv$Sex == 'f'){
        val_indv$stature[i] <- val_indv$female[i]
      } else if (val_indv$Sex == 'm') {
        val_indv$stature[i] <- val_indv$male[i]
      } else {
        val_indv$stature[i] <- val_indv$indet[i]
      }
    }
    rm(Fem1)
    next
  }
    
    # Check if value Tib1 was present and if so insert it in respective function
    if (exists("Tib1")){
      K_vercellotti_2009_m <- (Tib1 * 2.91) + 631
      K_vercellotti_2009_f <- (Tib1 * 2.79) + 614
      rm(Tib1)
      # Store results in data frame for every individual
      val_indv$male[i] <- K_vercellotti_2009_m
      val_indv$female[i] <- K_vercellotti_2009_f
      val_indv$indice[i] <- "Tib1"
      next
    } 
    
    # Hum1 
    if ("Hum1" %in% df_bones$variable){
      Hum1 <- df_bones$value[df_bones$variable == "Hum1"]
    } else if (("Hum1r" %in% df_bones$variable) & !("Hum1l" %in% df_bones$variable)){
      Hum1 <- df_bones$value[df_bones$variable == "Hum1r"]
    } else if (("Hum1l" %in% df_bones$variable) & !("Hum1r" %in% df_bones$variable)){
      Hum1 <- df_bones$value[df_bones$variable == "Hum1l"]
    } else if (("Hum1l" %in% df_bones$variable) & ("Hum1r" %in% df_bones$variable)){
      Hum1 <- (((df_bones$value[df_bones$variable == "Hum1l"])+(df_bones$value[df_bones$variable == "Hum1r"]))/2)
    }
    
    # Check if value Hum1 was present and if so insert it in respective function
    if (exists("Hum1")){
      K_vercellotti_2009_m <- (Hum1 * 3.11) + 677
      K_vercellotti_2009_f <- (Hum1 * 3.11) + 630
      rm(Hum1)
      # Store results in data frame for every individual
      val_indv$male[i] <- K_vercellotti_2009_m
      val_indv$female[i] <- K_vercellotti_2009_f
      val_indv$indice[i] <- "Hum1"
      next
    } 
    
    # Rad1 
    if ("Rad1" %in% df_bones$variable){
      Rad1 <- df_bones$value[df_bones$variable == "Rad1"]
    } else if (("Rad1r" %in% df_bones$variable) & !("Rad1l" %in% df_bones$variable)){
      Rad1 <- df_bones$value[df_bones$variable == "Rad1r"]
    } else if (("Rad1l" %in% df_bones$variable) & !("Rad1r" %in% df_bones$variable)){
      Rad1 <- df_bones$value[df_bones$variable == "Rad1l"]
    } else if (("Rad1l" %in% df_bones$variable) & ("Rad1r" %in% df_bones$variable)){
      Rad1 <- (((df_bones$value[df_bones$variable == "Rad1l"])+(df_bones$value[df_bones$variable == "Rad1r"]))/2)
    }
    
    # Check if value Rad1 was present and if so insert it in respective function
    if (exists("Rad1")){
      K_vercellotti_2009_m <- (Rad1 * 1.92) + 1230
      K_vercellotti_2009_f <- (Rad1 * 3.45) + 785
      rm(Rad1)
      # Store results in data frame for every individual
      val_indv$male[i] <- K_vercellotti_2009_m
      val_indv$female[i] <- K_vercellotti_2009_f
      val_indv$indice[i] <- "Rad1"
      next
    } 
  }
  
  if (dim(val_indv)[1] == 0) {
    print("There is no usable bone measurement / indice available for the chosen formula")
  }
  
  rm(i, vec_indv)

  return(val_indv)
}
