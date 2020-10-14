#' Calculate body height indices and mean body height based on Vercellotti et al. 2009
#' 
#' Based on the available measurements of different bones of different individuals, the body height indices for males and females are calculated according to Vercelotti et al. 2009. Additionally the mean body height for female or male individuals are calculated on all usable indices.
#'
#' 
#' @title vercellotti_et_al_2009
#' 
#' @param df data.frame, containing informations on individual, bone and measurement
#'  
#' @return list, containing one data.frame with all calculated indices for every individual and a second data.frame with the mean body height for all individuals
#'           
#' @author Anna Loy <\email{aloy@@roots.uni-kiel.de}>
#' @author Nils Müller-Scheeßel <\email{nils.mueller-scheessel@@ufg.uni-kiel.de}>
#' @author Hendrik Raese <\email{h.raese@@ufg.uni-kiel.de}>
#' @author Christoph Rinne <\email{crinne@@ufg.uni-kiel.de}>
#' 
#' @examples
#' 
#'@export


vercellotti_et_al_2009 <- function(df){
  
  vec_indv <- unique(df$Individual) # extract names and quantity of unique individuals
  
  # Initialize data frame for later storage of different mean body heights
  val_indv <- as.data.frame(matrix(ncol=3, nrow=length(vec_indv)), row.names=vec_indv)
  colnames(val_indv) <-c("male","female","indice")
  
  # check available values for different variables needed for 
  for (i in 1:length(vec_indv)){
    df_knochen <- subset(df, subset=df$Individual == vec_indv[i])
  
    # F2 
    if ("F2" %in% df_knochen$variable){
      F2 <- df_knochen$value[df_knochen$variable == "F2"]
    } else if (("F2_r" %in% df_knochen$variable) & !("F2_l" %in% df_knochen$variable)){
      F2 <- df_knochen$value[df_knochen$variable == "F2_r"]
    } else if (("F2_l" %in% df_knochen$variable) & !("F2_r" %in% df_knochen$variable)){
      F2 <- df_knochen$value[df_knochen$variable == "F2_l"]
    } else if (("F2_l" %in% df_knochen$variable) & ("F2_r" %in% df_knochen$variable)){
      F2 <- (((df_knochen$value[df_knochen$variable == "F2_l"])+(df_knochen$value[df_knochen$variable == "F2_r"]))/2)
    }

    # T1 
    if ("T1" %in% df_knochen$variable){
      T1 <- df_knochen$value[df_knochen$variable == "T1"]
    } else if (("T1_r" %in% df_knochen$variable) & !("T1_l" %in% df_knochen$variable)){
      T1 <- df_knochen$value[df_knochen$variable == "T1_r"]
    } else if (("T1_l" %in% df_knochen$variable) & !("T1_r" %in% df_knochen$variable)){
      T1 <- df_knochen$value[df_knochen$variable == "T1_l"]
    } else if (("T1_l" %in% df_knochen$variable) & ("T1_r" %in% df_knochen$variable)){
      T1 <- (((df_knochen$value[df_knochen$variable == "T1_l"])+(df_knochen$value[df_knochen$variable == "T1_r"]))/2)
    }  
 
    # Check if values F2 and/or T1 were present and if so insert them in respective function
    
    if (exists("F2") & exists("T1")){
      K_vercellotti_2009_m <- ((F2+T1) * 1.50) + 469
      K_vercellotti_2009_f <- ((F2+T1) * 1.55) + 390
      rm(T1, F2)
      # Store results in data frame for every individual
      val_indv$male[i] <- K_vercellotti_2009_m
      val_indv$female[i] <- K_vercellotti_2009_f
      val_indv$indice[i] <- "F2/T1"
      next
    } else if (exists("F2")){
      K_vercellotti_2009_m <- (F2 * 2.70) + 481
      K_vercellotti_2009_f <- (F2 * 2.89) + 365
      val_indv$indice[i] <- "F2"
      rm(F2)
      # Store results in data frame for every individual
      val_indv$male[i] <- K_vercellotti_2009_m
      val_indv$female[i] <- K_vercellotti_2009_f
      next
    }
           
     # F1 
    if ("F1" %in% df_knochen$variable){
      F1 <- df_knochen$value[df_knochen$variable == "F1"]
    } else if (("F1_r" %in% df_knochen$variable) & !("F1_l" %in% df_knochen$variable)){
      F1 <- df_knochen$value[df_knochen$variable == "F1_r"]
    } else if (("F1_l" %in% df_knochen$variable) & !("F1_r" %in% df_knochen$variable)){
      F1 <- df_knochen$value[df_knochen$variable == "F1_l"]
    } else if (("F1_l" %in% df_knochen$variable) & ("F1_r" %in% df_knochen$variable)){
      F1 <- (((df_knochen$value[df_knochen$variable == "F1_l"])+(df_knochen$value[df_knochen$variable == "F1_r"]))/2)
    }
    
    # Check if value F1 was present and if so insert it in respective function
    if (exists("F1")){
      K_vercellotti_2009_m <- (F1 * 2.61) + 515
      K_vercellotti_2009_f <- (F1 * 2.89) + 353
      rm(F1)
      # Store results in data frame for every individual
      val_indv$male[i] <- K_vercellotti_2009_m
      val_indv$female[i] <- K_vercellotti_2009_f
      val_indv$indice[i] <- "F1"
      next
    } 
    
    # Check if value T1 was present and if so insert it in respective function
    if (exists("T1")){
      K_vercellotti_2009_m <- (T1 * 2.91) + 631
      K_vercellotti_2009_f <- (T1 * 2.79) + 614
      rm(T1)
      # Store results in data frame for every individual
      val_indv$male[i] <- K_vercellotti_2009_m
      val_indv$female[i] <- K_vercellotti_2009_f
      val_indv$indice[i] <- "T1"
      next
    } 
    
    # H1 
    if ("H1" %in% df_knochen$variable){
      H1 <- df_knochen$value[df_knochen$variable == "H1"]
    } else if (("H1_r" %in% df_knochen$variable) & !("H1_l" %in% df_knochen$variable)){
      H1 <- df_knochen$value[df_knochen$variable == "H1_r"]
    } else if (("H1_l" %in% df_knochen$variable) & !("H1_r" %in% df_knochen$variable)){
      H1 <- df_knochen$value[df_knochen$variable == "H1_l"]
    } else if (("H1_l" %in% df_knochen$variable) & ("H1_r" %in% df_knochen$variable)){
      H1 <- (((df_knochen$value[df_knochen$variable == "H1_l"])+(df_knochen$value[df_knochen$variable == "H1_r"]))/2)
    }
    
    # Check if value H1 was present and if so insert it in respective function
    if (exists("H1")){
      K_vercellotti_2009_m <- (H1 * 3.11) + 677
      K_vercellotti_2009_f <- (H1 * 3.11) + 630
      rm(H1)
      # Store results in data frame for every individual
      val_indv$male[i] <- K_vercellotti_2009_m
      val_indv$female[i] <- K_vercellotti_2009_f
      val_indv$indice[i] <- "H1"
      next
    } 
    
    # R1 
    if ("R1" %in% df_knochen$variable){
      R1 <- df_knochen$value[df_knochen$variable == "R1"]
    } else if (("R1_r" %in% df_knochen$variable) & !("R1_l" %in% df_knochen$variable)){
      R1 <- df_knochen$value[df_knochen$variable == "R1_r"]
    } else if (("R1_l" %in% df_knochen$variable) & !("R1_r" %in% df_knochen$variable)){
      R1 <- df_knochen$value[df_knochen$variable == "R1_l"]
    } else if (("R1_l" %in% df_knochen$variable) & ("R1_r" %in% df_knochen$variable)){
      R1 <- (((df_knochen$value[df_knochen$variable == "R1_l"])+(df_knochen$value[df_knochen$variable == "R1_r"]))/2)
    }
    
    # Check if value R1 was present and if so insert it in respective function
    if (exists("R1")){
      K_vercellotti_2009_m <- (R1 * 1.92) + 1230
      K_vercellotti_2009_f <- (R1 * 3.45) + 785
      rm(R1)
      # Store results in data frame for every individual
      val_indv$male[i] <- K_vercellotti_2009_m
      val_indv$female[i] <- K_vercellotti_2009_f
      val_indv$indice[i] <- "R1"
    } 
  }
  
  if (dim(val_indv)[1] == 0) {
    print("There is no usable bone measurement / indice available for the chosen formula")
  }
  
  verc_2009 <- val_indv[order(as.numeric(row.names(val_indv))), ]
  
  rm(c_k, i, K_vercellotti_2009_f,K_vercellotti_2009_m, vec_indv, df_K, df_knochen, val_indv)
  
  return(verc_2009)
}
