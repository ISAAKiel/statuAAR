

tbl <- read.csv("testdata_formula.csv", sep = ",")

vec_indv <- unique(tbl$Individual)



# Nach Vercellotti u.a. 2009

# Initialize data frame for later storage of different Ks
df_K <- setNames(data.frame(matrix(ncol = 12, nrow = length(vec_indv))), c("K1", "K2", "K3", "K4", "K5", "K6", "K7", "K8", "K9", "K10", "K11", "K12"))
df_K[is.na(df_K)] <- 0

# Initialize data frame for later storage of different mean body heights
val_indv <- as.data.frame(matrix(ncol=2, nrow=length(vec_indv)), row.names=vec_indv)
colnames(val_indv) <-c("male","female")

# check available values for different variables needed for 
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

  c_k <- 0
  
# check if values were present and if so insert them in respective function
  
  if (exists("F2")){
    df_K$K2[i] <- (F2 * 2.70) + 481
    df_K$K8[i] <- (F2 * 2.89) + 365
    c_k <- c_k+1
    } 
  
  if (exists("F2") & exists("T1")){
    df_K$K1[i] <- ((F2+T1) * 1.50) + 469
    df_K$K7[i] <- ((F2+T1) * 1.55) + 390
    c_k <- c_k+1
    }

  if (exists("F1")){
    df_K$K3[i] <- (F1 * 2.61) + 515
    df_K$K9[i] <- (F1 * 2.89) + 353
    c_k <- c_k+1
    } 
  
  if (exists("T1")){
    df_K$K4[i] <- (T1 * 2.91) + 631
    df_K$K10[i] <- (T1 * 2.79) + 614
    c_k <- c_k+1
   } 
  
  if (exists("H1")){
    df_K$K5[i] <- (H1 * 3.11) + 677
    df_K$K11[i] <- (H1 * 3.11) + 630
    c_k <- c_k+1
    } 
  
  if (exists("R1")){
    df_K$K6[i] <- (R1 * 1.92) + 1230
    df_K$K12[i] <- (R1 * 3.45) + 785
    c_k <- c_k+1
    } 

  # Results are added up for male and female indviduals and the arithmetic mean is calculated. Values have to be divided by 1000 for m. Meter are chosen here for not implying an accurancy (as would be the case with mm or cm) that is not available by these calculations.  
    K_vercellotti_2009_m <- round((((rowSums(df_K[i,1:6]))/c_k)/1000),2)
    K_vercellotti_2009_f <- round((((rowSums(df_K[i,7:12]))/c_k)/1000),2)

  # Store results in data frame for every individual
    
    val_indv$male[i] <- K_vercellotti_2009_m
    val_indv$female[i] <- K_vercellotti_2009_f
    
  }

df_K <- df_K[order(as.numeric(row.names(df_K))), ]  
val_indv <- val_indv[order(as.numeric(row.names(val_indv))), ]
