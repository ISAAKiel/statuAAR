

tbl <- read.csv("../testdata_input", sep = "")

vec_indv <- unique(tbl$Individual)



# Nach Vercellotti u.a. 2009

val_indv <- as.data.frame(matrix(ncol=2, nrow=length(vec_indv)), row.names=vec_indv)
colnames(val_indv) <-c("male","female")

for (i in 1:length(vec_indv)){
  df_knochen <- subset(tbl, subset=tbl$Individual == vec_indv[i])
  
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

  # check if values were present and if so insert them in respective function
  
  c_k <- 0
  K1 <- 0
  K2 <- 0
  K3 <- 0
  K4 <- 0
  K5 <- 0
  K6 <- 0
  K7 <- 0
  K8 <- 0
  K9 <- 0
  K10 <- 0
  K11 <- 0
  K12 <- 0
  
  if (exists("F2")){
    K2 <- (F2 * 2.70) + 481
    K8 <- (F2 * 2.89) + 365
    c_k <- c_k+1
    } 
  
  if (exists("F2") & exists("T1")){
    K1 <- ((F2+T1) * 1.50) + 469
    K7 <- ((F2+T1) * 1.55) + 390
    c_k <- c_k+1
    }

  if (exists("F1")){
    K3 <- (F1 * 2.61) + 515
    K9 <- (F1 * 2.89) + 353
    c_k <- c_k+1
    } 
  
  if (exists("T1")){
    K4 <- (T1 * 2.91) + 631
    K10 <- (T1 * 2.79) + 614
    c_k <- c_k+1
   } 
  
  if (exists("H1")){
    K5 <- (H1 * 3.11) + 677
    K11 <- (H1 * 3.11) + 630
    c_k <- c_k+1
    } 
  
  if (exists("R1")){
    K6 <- (R1 * 1.92) + 1230
    K12 <- (R1 * 3.45) + 785
    c_k <- c_k+1
    } 

  # Results are added up for male and female indviduals and the arithmetic mean is calculated. Values have to be divided by 1000 for m. Meter are chosen here for not implying an accurancy (as would be the case with mm or cm) that is not available by these calculations.  
    K_vercellotti_2009_m <- round((((K1+K2+K3+K4+K5+K6)/c_k)/1000),2)
    K_vercellotti_2009_f <- round((((K7+K8+K9+K10+K11+K12)/c_k)/1000),2)

  # Store results in data frame for every individual
    val_indv$male[i] <- K_vercellotti_2009_m
    val_indv$female[i] <- K_vercellotti_2009_f
  
  # Store Ks in additional data frame -> to be done
    
  }
  
val_indv <- val_indv[order(as.numeric(row.names(val_indv))), ]
