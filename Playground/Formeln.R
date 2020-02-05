

tbl <- read.csv("../testdata_input", sep = "")

vec_indv <- unique(tbl$Individual)



# Nach Vercellotti u.a. 2009


i <- 1

for (i in i:length(vec_indv)){
  df_knochen <- subset(tbl, subset=tbl$Individual == vec_indv[i])

  # F1 (müsste das so aussehen?)
  if ("F1" %in% df_knochen$variable){
    F1 <- df_knochen$value[df_knochen$variable == "F1"]
  } else if (("F1_r" %in% df_knochen$variable) & !("F1_l" %in% df_knochen$variable)){
    F1 <- df_knochen$value[df_knochen$variable == "F1_r"]
  } else if (("F1_l" %in% df_knochen$variable) & !("F1_r" %in% df_knochen$variable)){
    F1 <- df_knochen$value[df_knochen$variable == "F1_l"]
  } else if (("F1_l" %in% df_knochen$variable) & ("F1_r" %in% df_knochen$variable)){
    F1 <- (((df_knochen$value[df_knochen$variable == "F1_l"])+(df_knochen$value[df_knochen$variable == "F1_r"]))/2)
  }
  
  # F2 (müsste das so aussehen?)
  if ("F2" %in% df_knochen$variable){
    F2 <- df_knochen$value[df_knochen$variable == "F2"]
  } else if (("F2_r" %in% df_knochen$variable) & !("F2_l" %in% df_knochen$variable)){
    F2 <- df_knochen$value[df_knochen$variable == "F2_r"]
  } else if (("F2_l" %in% df_knochen$variable) & !("F2_r" %in% df_knochen$variable)){
    F2 <- df_knochen$value[df_knochen$variable == "F2_l"]
  } else if (("F2_l" %in% df_knochen$variable) & ("F2_r" %in% df_knochen$variable)){
    F2 <- (((df_knochen$value[df_knochen$variable == "F2_l"])+(df_knochen$value[df_knochen$variable == "F2_r"]))/2)
  }
  
  
  # H1 (müsste das so aussehen?)
  if ("H1" %in% df_knochen$variable){
    H1 <- df_knochen$value[df_knochen$variable == "H1"]
  } else if (("H1_r" %in% df_knochen$variable) & !("H1_l" %in% df_knochen$variable)){
    H1 <- df_knochen$value[df_knochen$variable == "H1_r"]
  } else if (("H1_l" %in% df_knochen$variable) & !("H1_r" %in% df_knochen$variable)){
    H1 <- df_knochen$value[df_knochen$variable == "H1_l"]
  } else if (("H1_l" %in% df_knochen$variable) & ("H1_r" %in% df_knochen$variable)){
    H1 <- (((df_knochen$value[df_knochen$variable == "H1_l"])+(df_knochen$value[df_knochen$variable == "H1_r"]))/2)
  }
    
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

  # T1 (müsste das so aussehen?)
  if ("T1" %in% df_knochen$variable){
    T1 <- df_knochen$value[df_knochen$variable == "T1"]
  } else if (("T1_r" %in% df_knochen$variable) & !("T1_l" %in% df_knochen$variable)){
    T1 <- df_knochen$value[df_knochen$variable == "T1_r"]
  } else if (("T1_l" %in% df_knochen$variable) & !("T1_r" %in% df_knochen$variable)){
    T1 <- df_knochen$value[df_knochen$variable == "T1_l"]
  } else if (("T1_l" %in% df_knochen$variable) & ("T1_r" %in% df_knochen$variable)){
    T1 <- (((df_knochen$value[df_knochen$variable == "T1_l"])+(df_knochen$value[df_knochen$variable == "T1_r"]))/2)
  }  

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
  
# nächste Schritte: Werte zusammenrechnen und richtigen Durchschnitt bestimmen (Anzahl der vorhandenen Kx-Werte beachten), Durchschnittswerte nach Geschlecht dem Individuum zuweisen -> dann zum nächsten Individuum gehen

  }
  



