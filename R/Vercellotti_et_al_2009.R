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
#' @author Christoph Rinne <\email{	crinne@@ufg.uni-kiel.de}>
#' 
#' @examples
#' 
#'@export


vercellotti_et_al_2009 <- function(df){
  
  vec_indv <- unique(df$Individual) # extract names and quantity of unique individuals
  
  #  Initialize data frame for later storage of different Ks
 # df_K <- setNames(data.frame(matrix(ncol = 12, nrow = length(vec_indv))), c("K1", "K2", "K3", "K4", "K5", "K6", "K7", "K8", "K9", "K10", "K11", "K12"))
#  df_K[is.na(df_K)] <- 0
  
  # Initialize data frame for later storage of different mean body heights
  val_indv <- as.data.frame(matrix(ncol=2, nrow=length(vec_indv)), row.names=vec_indv)
  colnames(val_indv) <-c("male","female")
  
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
 
    if (exists("F2") & exists("T1")){
      K_vercellotti_2009_m <- ((F2+T1) * 1.50) + 469
      K_vercellotti_2009_f <- ((F2+T1) * 1.55) + 390
      rm(T1, F2)
    } # Check if values were present and if so insert them in respective function
           
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
    

    
    

    
      else if (exists("F2")){
        K_vercellotti_2009_m <- (F2 * 2.70) + 481
        K_vercellotti_2009_f <- (F2 * 2.89) + 365
        rm(F2)
      } 
    
      else if (exists("F1")){
        K_vercellotti_2009_m <- (F1 * 2.61) + 515
        K_vercellotti_2009_f <- (F1 * 2.89) + 353
        rm(F1)
      } 
    
      else if (exists("T1")){
        K_vercellotti_2009_m <- (T1 * 2.91) + 631
        K_vercellotti_2009_f <- (T1 * 2.79) + 614
        rm(T1)
      } 
    
      else if (exists("H1")){
        K_vercellotti_2009_m <- (H1 * 3.11) + 677
        K_vercellotti_2009_f <- (H1 * 3.11) + 630
        rm(H1)
      } 
    
      else if (exists("R1")){
        K_vercellotti_2009_m <- (R1 * 1.92) + 1230
        K_vercellotti_2009_f <- (R1 * 3.45) + 785
        rm(R1)
      } 
    
    # Results are added up for male and female individuals and the arithmetic mean is calculated. Values have to be divided by 1000 for showing results in meter. Meter are chosen here to not imply an accuracy (as would be the case with mm or cm) that is not available by these calculations.  
#    K_vercellotti_2009_m <- round((((rowSums(df_K[i,1:6]))/c_k)/1000),2)
#    K_vercellotti_2009_f <- round((((rowSums(df_K[i,7:12]))/c_k)/1000),2)
    
    # Store results in data frame for every individual
    val_indv$male[i] <- K_vercellotti_2009_m
    val_indv$female[i] <- K_vercellotti_2009_f
    
  }
  
#  df_K <- df_K[order(as.numeric(row.names(df_K))), ]  
  val_indv <- val_indv[order(as.numeric(row.names(val_indv))), ]

  verc_2009 <- list("indices"=df_K, "height"=val_indv)
  
  rm(c_k, i, K_vercellotti_2009_f,K_vercellotti_2009_m, vec_indv, df_K, df_knochen, val_indv)
  
  return(verc_2009)
}
