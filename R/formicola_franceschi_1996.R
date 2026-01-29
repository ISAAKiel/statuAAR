#' @name formicola_franceschi_1996
#'
#' @title Calculate stature estimation based on bone measures according to: Formicola & Franceschi 1996.
#'
#' @description
#' Stature estimation (mm) based on a hierarchical order of regression calculations,
#' separated  by sex and based on: Fem2+Tib1, Fem1, Tib1, Hum1, Rad1.
#' If bone measures for left and right are provided the mean value will be used,
#' but for statistic information 2 bones will be counted (n_measures).
#' If sex is indet. the mean of male and female stature estimation is given.
#' Formicola and Franceschi (1996) do not provide information on hierarchie or mean calculation
#' of the regression formula, but table 3 gives standard error (S.E.) and correlation
#' from which a hierarchical order can be derived. The order of bone measures used
#' according to achieved correlation differs between sex and will be respected,
#' allthough  differences are small. Due to this, the order used differs slightly from
#' Siegmund (2010, 116 (12.8)).
#'
#' Bone measures used: Fem2+Tib1, Tib1, Fem1, Hum1, Rad1 (or Rad1a)
#'
#' Returns a data.frame with:
#' \itemize{
#' \item{ ind: individual identifyer (rownames), }
#' \item{ sex: as provided for calculation: m, f, indet.}
#' \item{ stature: estimated on the provided sex and bone measures, }
#' \item{ bone (measure(s)): bones used for calculation, }
#' \item{ female (stature): columns with alternative stature for three sex classes, }
#' \item{ male (stature), }
#' \item{ indet. (stature) and}
#' \item{ n_measures: number of bone measures included: e.g. 2 Fem2 (left, right), 1 Tib1}
#' }
#'
#' @param df data.frame of type statuaar_data_table, containing informations on individual, bone and measurement.
#'
#' @return data.frame with calculated stature and related information per individual.
#'
#' @author Christoph Rinne \email{crinne@@ufg.uni-kiel.de}
#'
#' @examples
#' # Read example dataset into a data frame
#'
#' @references
#'   \insertRef{Formicola_Franceschi_1996}{statuAAR}
#'
#'   \insertRef{Formicola_1993}{statuAAR}
#'
#'   \insertRef{Siegmund_2010}{statuAAR}
#'
#' @export

formicola_franceschi_1996 <- function(df){

  df$variable<-gsub("([rl]$)","", df$variable) # laterality not needed

    # check if needed measures are present
  needed <- getFormulaMeasures('ff96')
  if (!any(df$variable %in% needed)){
    return("There is no usable bone measurement / indice available for the chosen formula.")
  }

  # aggregate values for each measure and individual
  df <- aggregate(value ~ Ind + Sex + variable,
                  data = df,
                  FUN = function(x) c(mean = mean(x), n = length(x)))
  df <- do.call(data.frame, df)

  vec_indv <- unique(df$Ind) # extract names and quantity of unique individuals

  # Initialize data frame for later storage of different mean body heights
  val_indv <- as.data.frame(matrix(ncol=7, nrow=length(vec_indv)), row.names=vec_indv)
  colnames(val_indv) <-c("sex", "stature", "bone", "female", "male", "indet", "n_measures")
  val_indv$sex <- factor(val_indv$sex, labels = c("m", "f", "indet"), levels = c(1,2,3))

    # Calculte in hierarchical order

    # check available values for different variables needed for
    for (i in 1:length(vec_indv)){
      df_bones <- subset(df, subset=df$Ind == vec_indv[i])

      # get all optional needed measures
      Fem2 <- df_bones$value.mean[df_bones$variable=="Fem2"]
      Tib1 <- df_bones$value.mean[df_bones$variable=="Tib1"]
      Fem1 <- df_bones$value.mean[df_bones$variable=="Fem1"]
      Rad1 <- df_bones$value.mean[df_bones$variable=="Rad1"]
      Rad1a <- FALSE
      if (length(Rad1)<2){
        Rad1 <- df_bones$value.mean[df_bones$variable=="Rad1a"]
        Rad1a <- TRUE
      }
      Hum1 <- df_bones$value.mean[df_bones$variable=="Hum1"]

      # check for different combinations of measures
      # Fem2 & Tib1
      if (length(Fem2)>0 & length(Tib1)>0){
        stature.m <- ((Fem2 + Tib1) * 1.30) + 604.2
        stature.f <- ((Fem2 + Tib1) * 1.33) + 545.7
        statures <- c(stature.m, stature.f, mean(c(stature.m, stature.f)))
        indice <- "1. Fem2&Tib1"
        n_measures <- df_bones$value.n[df_bones$variable=="Fem2"] +
          df_bones$value.n[df_bones$variable=="Tib1"]
        # Fem1
      } else if (length(Fem1)>0) {
        stature.m <- (Fem1 * 2.55) + 520.8
        stature.f <- (Fem1 * 2.61) + 460.5
        statures <- c(stature.m, stature.f, mean(c(stature.m, stature.f)))
        indice <- "2. Fem1"
        n_measures <- df_bones$value.n[df_bones$variable=="Fem1"]
        # Tib1
      } else if (length(Tib1)>0) {
        stature.m <- (Tib1 * 2.79) + 634.1
        stature.f <- (Tib1 * 2.80) + 595.8
        statures <- c(stature.m, stature.f, mean(c(stature.m, stature.f)))
        indice <- "3. Tib1"
        n_measures <- df_bones$value.n[df_bones$variable=="Tib1"]
        # Fem1
      } else if (length(Hum1)>0) {
        stature.m <- (Hum1 * 4.04) + 380.5
        stature.f <- (Hum1 * 3.75) + 446.4
        statures <- c(stature.m, stature.f, mean(c(stature.m, stature.f)))
        indice <- "4. Hum1"
        n_measures <- df_bones$value.n[df_bones$variable=="Hum1"]
        # Rad1
      } else if (length(Rad1)>0) {
        stature.m <- (Rad1 * 4.38) + 579.0
        stature.f <- (Rad1 * 3.98) + 651.2
        statures <- c(stature.m, stature.f, mean(c(stature.m, stature.f)))
        indice <- "5. Rad1"
        n_measures <- df_bones$value.n[df_bones$variable=="Rad1"]
        if(Rad1a){
          indice <- "5. Rad1a"
          n_measures <- df_bones$value.n[df_bones$variable=="Rad1a"]
        }
      } else {
        # no apropriate measures given
        statures <-rep(NA, 3)
        indice <- NA
        n_measures <- 0
      }

      statures <- round(statures, 0)

    # write values into data frame of results
    val_indv$sex[i] <- unique(df_bones$Sex)
    val_indv$stature[i] <- statures[as.integer(unique(df_bones$Sex))]
    val_indv$bone[i] <- indice
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
