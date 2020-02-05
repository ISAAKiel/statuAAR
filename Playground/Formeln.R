tbl <- read.csv("../testdata_input", sep = "")

vec_indv <- unique(tbl$Individual)

i <- 1

for (i in i:length(vec_indv)){
  df_knochen <- subset(tbl, subset=tbl$Individual == vec_indv[i])
  
  # R1 (müsste das so aussehen?)
  if ("R1" %in% df_knochen$variable){
  R1 <- df_knochen$value[df_knochen$variable == "R1"]
  } else if (("R1_r" %in% df_knochen$variable) & !("R1_l" %in% df_knochen$variable)){
    R1 <- df_knochen$value[df_knochen$variable == "R1_r"]
  } else if (("R1_l" %in% df_knochen$variable) & !("R1_r" %in% df_knochen$variable)){
    R1 <- df_knochen$value[df_knochen$variable == "R1_l"]
  } else if (("R1_l" %in% df_knochen$variable) & ("R1_r" %in% df_knochen$variable)){
    R1 <- (((df_knochen$value[df_knochen$variable == "R1_l"])+(df_knochen$value[df_knochen$variable == "R1_r"]))/2)
  }
 
  # R2 (müsste das so aussehen?)
  if ("R2" %in% df_knochen$variable){
    R2 <- df_knochen$value[df_knochen$variable == "R2"]
  } else if (("R2_r" %in% df_knochen$variable) & !("R2_l" %in% df_knochen$variable)){
    R2 <- df_knochen$value[df_knochen$variable == "R2_r"]
  } else if (("R2_l" %in% df_knochen$variable) & !("R2_r" %in% df_knochen$variable)){
    R2 <- df_knochen$value[df_knochen$variable == "R2_l"]
  } else if (("R2_l" %in% df_knochen$variable) & ("R2_r" %in% df_knochen$variable)){
    R2 <- (((df_knochen$value[df_knochen$variable == "R2_l"])+(df_knochen$value[df_knochen$variable == "R2_r"]))/2)
  }
   
  F1 <- df_knochen$value[df_knochen$variable == "F1"]
  H1 <- df_knochen$value[df_knochen$variable == "H1"]
  tbl_val <- rbind.data.frame(c(R1,F1,H1),deparse.level = 1)
}
  

# Nach Vercellotti u.a. 2009
# Ergebnis muss durch 10 dividiert werden, um cm zu erhalten!

# Männer
K1 <- ((F2+T1) * 1.50) + 469
K2 <- (F2 * 2.70) + 481
K3 <- (F1 * 2.61) + 515
K4 <- (T1 * 2.91) + 631
K5 <- (H1 * 3.11) + 677
K6 <- (R1 * 1.92) + 1230

# Frauen
K7 <- ((F2+T1) * 1.55) + 390
K8 <- (F2 * 2.89) + 365
K9 <- (F1 * 2.89) + 353
K10 <- (T1 * 2.79) + 614
K11 <- (H1 * 3.11) + 630
K12 <- (R1 * 3.45) + 785
