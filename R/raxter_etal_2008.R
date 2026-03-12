#' @name raxter_etal_2008
#'
#' @title Calculate stature estimation according to: Raxter el al 2008.
#'
#' @description
#' Stature estimation (mm) based on the hierarchy of different regression calculations,
#' separated  by sex, based on Egyptian skeletal sample consists of 63 adult males
#' and 37 adult females (Raxter el al 2008).
#'
#' Bone measures used in hierarchical order of correlation (r):
#' \itemize{
#' \item{ male: Fem1+Tib1a, Fem2+Tib1a, Tib1a, Tib1b, Fem1, Fem2, Hum1+Rad1, Hum1, Rad1 }
#' \item{ female: Tib1b, Tib1a, Fem2+Tib1b, Fem1+Tib1a, Fem2, Fem1, Hum1, Hum1+Rad1, Rad1 }
#' }
#'
#' The addition of individual bone measurements is performed during the calculation.
#' If bone measures for left and right are provided the mean value will be used,
#' but for statistic information 2 bones will be counted (n_measures).
#' If sex is indet. the mean of male and female stature estimation is given.
#' The authors do not mention, if the mean of all regressions is used or the
#' first possible stature estimation according to hierarchical order of
#' correlation. As the authors discuss the correlation and refere to the
#' work of Trotter & Gleser a hierarchical order is supposed (s. Siegmund 2010,
#' p. 20). As correlation within male and females is highly different and provides
#' a sex specific order of regressions the stature of individuals with
#' indeterminated sex (indet.) is calculated by the mean of both at each rank
#' mentioning all bone measures used and the sum of n_measures.
#'
#' Returns a data.frame with:
#' \itemize{
#' \item{ ind: individual identifyer (rownames), }
#' \item{ sex: as provided for calculation: m, f, indet. }
#' \item{ stature: estimated on the provided sex and bone measures, }
#' \item{ bone (measure(s)): bones used for calculation, }
#' \item{ if_female (stature): columns with alternative stature for three sex classes, }
#' \item{ if_male (stature), }
#' \item{ if_indet. (stature) and }
#' \item{ n_measures: number of bone measures included:
#'              e.g. 2 Fem2 (left, right) + 1 Tib1 }
#' }
#'
#' @param df data.frame of type statuaar_data_table, containing informations on individual, bone and measurement.
#'
#' @return data.frame with calculated stature and related information per individual.
#'
#' @author Christoph Rinne \email{crinne@@ufg.uni-kiel.de}
#' @author Hendrik Raese \email{h.raese@@ufg.uni-kiel.de}
#'
#' @references
#'   \insertRef{Raxter_Ruff_Azab_Erfan_Soliman_El-Sawaf_2008}{statuAAR}
#'
#'   \insertRef{Siegmund_2010}{statuAAR}
#'
#'   \insertRef{Trotter_Gleser_1952}{statuAAR}
#'
#'   \insertRef{Trotter_Gleser_1977}{statuAAR}
#'
#' @examples
#' # Read example dataset into a data frame.
#' # Raxter et al. compare their data with Trotter & Gleser 1952 (Black)
#' x <- statuAAR::TrotterGleser1952
#' x <- x[x$Race == "Black", ]
#'
#' # Create & check the data frame of mesures concordance for Trotter & Gleser 1952
#' measures.concordance <- create.measures.concordance()
#' measures.concordance[measures.concordance$own != "",]
#'
# Prepare statuaar_data_table
#' dl.tgb <- statuAAR::prep.statuaar.data(x, d.form = "wide", ind = "Appendix_row",
#'                                        sex = "Sex", measures.names = "own", stats = FALSE)
#'
#' # Calculate stature estimation using a given formula.
#' # (Present: Tib1, required: Tib1a)
#' statuAAR::getStature(c("ra08"), dl.tgb)
#' # raxter_etal_2008(dl.tgb) # The alternative.
#'
#'@export

###################################
raxter_etal_2008 <- function(df){

#######################################################
# Sub function for male stature (s.b.)

  calc.stature.m <- function (df_bones) {

    # get all optional needed measures
    Fem1 <- df_bones$value.mean[df_bones$variable == "Fem1"]
    Fem2 <- df_bones$value.mean[df_bones$variable == "Fem2"]
    Tib1a <- df_bones$value.mean[df_bones$variable == "Tib1a"]
    Tib1b <- df_bones$value.mean[df_bones$variable == "Tib1b"]
    Hum1 <- df_bones$value.mean[df_bones$variable == "Hum1"]
    Rad1 <- df_bones$value.mean[df_bones$variable == "Rad1"]

    stature.m <- c()

    stature.m <- ((Fem1 + Tib1a) * 1.282) + 593.5
    indice <- "Fem1+Tib1a"
    n_measures <- df_bones$value.n[df_bones$variable == "Fem1"] +
      df_bones$value.n[df_bones$variable == "Tib1a"]
    if (length(stature.m)==0){
      stature.m <- ((Fem2 + Tib1a) * 1.276) + 606.4
      indice <- "Fem2+Tib1a"
      n_measures <- df_bones$value.n[df_bones$variable == "Fem2"] +
        df_bones$value.n[df_bones$variable == "Tib1a"]
    }
    if (length(stature.m)==0){
      stature.m <- (Tib1a * 2.554) + 692.1
      indice <- "Tib1a"
      n_measures <- df_bones$value.n[df_bones$variable == "Tib1a"]
    }
    if (length(stature.m)==0){
      stature.m <- (Tib1b* 2.552) + 701.8
      indice <- "Tib1b"
      n_measures <- df_bones$value.n[df_bones$variable == "Tib1b"]
    }
    if (length(stature.m)==0){
      stature.m <- (Fem1 * 2.257) + 639.3
      indice <- "Fem1"
      n_measures <- df_bones$value.n[df_bones$variable == "Fem1"]
    }
    if (length(stature.m)==0){
      stature.m <- (Fem2 * 2.253) + 647.6
      indice <- "Fem2"
      n_measures <- df_bones$value.n[df_bones$variable == "Fem2"]
    }
    if (length(stature.m)==0){
      stature.m <- ((Hum1 + Rad1) * 1.456) + 837.6
      indice <- "Hum1&Rad1"
      n_measures <- df_bones$value.n[df_bones$variable == "Rad1"] +
        df_bones$value.n[df_bones$variable == "Hum1"]
    }
    if (length(stature.m)==0){
      stature.m <- (Hum1 * 2.594) + 838.5
      indice <- "Hum1"
      n_measures <- df_bones$value.n[df_bones$variable == "Hum1"]
    }
    if (length(stature.m)==0){
      stature.m <- (Rad1 * 2.641) + 1009.1
      indice <- "Rad1"
      n_measures <- df_bones$value.n[df_bones$variable == "Rad1"]
    }
    # End of male stature estimation

    return(list("stature"=stature.m, "indice"=indice, "n_measures"=n_measures))

  } # End of Function calc.stature.m

##############################################################
# Sub function for female stature (s.b.)

  calc.stature.f <- function (df_bones) {

    # get all optional needed measures
    Fem1 <- df_bones$value.mean[df_bones$variable == "Fem1"]
    Fem2 <- df_bones$value.mean[df_bones$variable == "Fem2"]
    Tib1a <- df_bones$value.mean[df_bones$variable == "Tib1a"]
    Tib1b <- df_bones$value.mean[df_bones$variable == "Tib1b"]
    Hum1 <- df_bones$value.mean[df_bones$variable == "Hum1"]
    Rad1 <- df_bones$value.mean[df_bones$variable == "Rad1"]

    stature.f <- c()

    stature.f <- (Tib1b * 2.700) + 618.9
    indice <- "Tib1b"
    n_measures <- df_bones$value.n[df_bones$variable == "Tib1b"]
    if (length(stature.f)==0){
      stature.f <- (Tib1a * 2.699) + 610.8
      indice <- "Tib1a"
      n_measures <- df_bones$value.n[df_bones$variable == "Tib1a"]
    }
    if (length(stature.f)==0){
      stature.f <- ((Fem2 + Tib1b) * 1.312) + 552.7
      indice <- "Fem2+Tib1b"
      n_measures <- df_bones$value.n[df_bones$variable == "Tib1b"] +
        df_bones$value.n[df_bones$variable == "Fem2"]
    }
    if (length(stature.f)==0){
      stature.f <- ((Fem1 + Tib1a) * 1.313) + 543.6
      indice <- "Fem1+Tib1a"
      n_measures <- df_bones$value.n[df_bones$variable == "Tib1a"] +
        df_bones$value.n[df_bones$variable == "Fem1"]
    }
    if (length(stature.f)==0){
      stature.f <- (Fem2 * 2.341) + 576.3
      indice <- "Fem2"
      n_measures <- df_bones$value.n[df_bones$variable == "Fem2"]
    }
    if (length(stature.f)==0){
      stature.f <- (Fem1 * 2.340) + 569.9
      indice <- "Fem1"
      n_measures <- df_bones$value.n[df_bones$variable == "Fem1"]
    }
    if (length(stature.f)==0){
      stature.f <- (Hum1 * 2.827) + 709.4
      indice <- "Hum1"
      n_measures <- df_bones$value.n[df_bones$variable == "Hum1"]
    }
    if (length(stature.f)==0){
      stature.f <- ((Hum1 + Rad1) * 1.291) + 864.1
      indice <- "Hum1+Rad1"
      n_measures <- df_bones$value.n[df_bones$variable == "Hum1"] +
        df_bones$value.n[df_bones$variable == "Rad1"]
    }
    if (length(stature.f)==0){
      stature.f <- (Rad1 * 2.509) + 967.3
      indice <- "Rad1"
      n_measures <- df_bones$value.n[df_bones$variable == "Rad1"]
    }
    # End of male stature estimation

    return(list("stature"=stature.f, "indice"=indice, "n_measures"=n_measures))

  } # End of Function calc.stature.f

#################################################
# Data preparation & calculation

  df$variable <- gsub("([rl]$)", "", df$variable) # laterality not needed
  # check if needed measures are present
  needed <- getFormulaMeasures('ra08')
  if (!any(df$variable %in% needed)){
    return("There is no usable bone measurement / indice available for the chosen formula.")
  }

  # aggregate values for each measure and individual
  df <- stats::aggregate(value ~ Ind + Sex + variable,
                  data = df,
                  FUN = function(x) c(mean = mean(x), n = length(x)))
  df <- do.call(data.frame, df)

  vec_indv <- unique(df$Ind) # extract names and quantity of unique individuals

  # Initialize data frame for later storage of different mean body heights
  val_indv <- as.data.frame(matrix(ncol = 7, nrow = length(vec_indv)), row.names = vec_indv)
  colnames(val_indv) <- c("sex", "stature", "bone", "if_female", "if_male", "if_indet", "n_measures")
  val_indv$sex <- factor(val_indv$sex, labels = c("m", "f", "indet"), levels = c(1, 2, 3))


  # check available values for different variables needed for
  for (i in seq_along(vec_indv)){
    df_bones <- subset(df, subset = Ind == vec_indv[i])

    stature.m <- calc.stature.m(df_bones)
    stature.f <- calc.stature.f(df_bones)

    #  mean for indet. sex,  all bone labels and sum of n_measures

    stature.i <- mean(c(stature.m[[1]], stature.f[[1]]))
    indice.i <- paste(stature.m[[2]], stature.f[[2]], sep = ", ")
    n_measures.i <- stature.m[[3]] + stature.f[[3]]

    # vectors for stature estimations, indices and n_measures
    statures <- c(stature.m[[1]], stature.f[[1]], stature.i)
    statures <- round(statures, 0)
    indices <- c(stature.m[[2]], stature.f[[2]], indice.i)
    n_measures <- c(stature.m[[3]], stature.f[[3]], n_measures.i)

    # write values into data frame of results
    val_indv$sex[i] <- unique(df_bones$Sex)
    val_indv$stature[i] <- statures[as.integer(unique(df_bones$Sex))]
    val_indv$bone[i] <- indices[as.integer(unique(df_bones$Sex))]
    val_indv$if_female[i] <- statures[2]
    val_indv$if_male[i] <- statures[1]
    val_indv$if_indet[i] <- statures[3]
    val_indv$n_measures[i] <- n_measures[as.integer(unique(df_bones$Sex))]
  } # next individual

  if (dim(val_indv)[1] == 0) {
    print("There is no usable bone measurement / indice available for the chosen formula")
  }

  return(val_indv)
}

