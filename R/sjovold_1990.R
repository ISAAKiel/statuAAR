#' @name sjovold_1990
#'
#' @title Calculate stature estimation according to: Sjøvold 1990.
#'
#' @description
#' Stature estimation (mm) based on the mean of different regression calculations,
#' not separated  by sex (Sjøvold 1990).
#' Bone measures used: Hum1, Rad1 (alt. Rad1b), Uln1, Fem1 (alt. Fem2), Fib1.

#' If bone measures for left and right are provided the mean value will be used,
#' but for statistic information 2 bones will be counted (n_measures).
#' The author discusses the correlation and disadvantages of the various regression
#' calculations. Only the calculations based on the tibia are rejected and are
#' therefore not calculated here. All other regressions appear to be equally valid to him.
#' Allthough not explained in the text estimated stature is derived from the mean
#' of all calculations from the bone measures excluding Tibia. To not multiply
#' significance in the case of two measures per bone (e.g. Fem 1, Fem2) only one
#' of both will be calculated.
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
#' \item{ n_measures: number of bone measures included:
#'              e.g. 2 Fem2 (left, right) + 1 Tib1}
#' }
#'
#' @param df data.frame of type statuaar_data_table, containing informations on individual, bone and measurement.
#'
#' @return data.frame with calculated stature and related information per individual.
#'
#' @author Christoph Rinne \email{crinne@@ufg.uni-kiel.de}
#'
#' @references
#'   \insertRef{Sjøvold_1990}{statuAAR}
#'
#' @examples
#' # Read example dataset into a data frame
#'
#' @export

sjovold_1990 <- function(df){

  df$variable<-gsub("([rl]$)","", df$variable) # laterality not needed

  # check if needed measures are present
  needed <- getFormulaMeasures('sjovold_1990')
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

  # check available values for different variables needed for
  for (i in 1:length(vec_indv)){
    df_bones <- subset(df, subset=df$Ind == vec_indv[i])
    # Get measure values needed
    Hum1 <- df_bones$value.mean[df_bones$variable=="Hum1"]
    Rad1 <- df_bones$value.mean[df_bones$variable=="Rad1"]
    Rad1b <- df_bones$value.mean[df_bones$variable=="Rad1b"]
    Uln1 <- df_bones$value.mean[df_bones$variable=="Uln1"]
    Fem1 <- df_bones$value.mean[df_bones$variable=="Fem1"]
    Fem2 <- df_bones$value.mean[df_bones$variable=="Fem2"]
    #Tib1 <- df_bones$value.mean[df_bones$variable=="Tib1"]
    #Tib1b <- df_bones$value.mean[df_bones$variable=="Tib1b"]
    Fib1 <- df_bones$value.mean[df_bones$variable=="Fib1"]

    # calculate statuar and document bone measures and number used for calculation

    measures <- c()
    bone <- c()
    n_measures <- 0
    if (length(Hum1)>0){
      measures <- append(measures, (4.62 * Hum1) + 190.00)
      bone <- append(bone, "Hum1")
      n_measures <- n_measures + df_bones$value.n[df_bones$variable=="Hum1"]
    }
    if (length(Rad1)>0) {
      measures <- append(measures, (3.78 * Rad1) + 747.0)
      bone <- append(bone, "Rad1")
      n_measures <- n_measures + df_bones$value.n[df_bones$variable=="Rad1"]
    }else if (length(Rad1b)>0){
      measures <- append(measures, (4.80 * Rad1b) + 515.5)
      bone <- append(bone, "Rad1")
      n_measures <- n_measures + df_bones$value.n[df_bones$variable=="Rad1b"]
    }
    if (length(Uln1)>0) {
      measures <- append(measures, (4.61 * Uln1) + 468.3)
      bone <- append(bone, "Uln1")
      n_measures <- n_measures + df_bones$value.n[df_bones$variable=="Uln1"]
    }
    if (length(Fem1)>0) {
      measures <- append(measures, (2.71 * Fem1) + 458.6)
      bone <- append(bone, "Fem1")
      n_measures <- n_measures + df_bones$value.n[df_bones$variable=="Fem1"]
    } else if (length(Fem2)>0){
      measures <- append(measures, (3.01 * Fem2) + 325.2)
      bone <- append(bone, "Fem2")
      n_measures <- n_measures + df_bones$value.n[df_bones$variable=="Fem2"]
    }
#    if (length(Tib1)>0) {
#      measures <- append(measures, (3.29 * Tib1) + 473.4)
#      bone <- append(bone, "Tib1")
#      n_measures <- n_measures + df_bones$value.n[df_bones$variable=="Tib1"]
#    } else if (length(Tib1b)>0){
#      measures <- append((3.67 * Tib1b) + 295.0)
#      bone <- append(bone, "Tib1b")
#      n_measures <- n_measures + df_bones$value.n[df_bones$variable=="Tib1b"]
#    }
    if (length(Fib1)>0) {
      measures <- append(measures, (3.59 * Fib2) + 363.1)
      bone <- append(bone, "Fib1")
      n_measures <- n_measures + df_bones$value.n[df_bones$variable=="Fib1"]
    }

    # calculate mean of each measures group for statures
    # as the regression equations are calculated in cm the result is converted to mm

    stature <- round(mean(measures), 0)

    # write values into data frame of results
    val_indv$sex[i] <- unique(df_bones$Sex)
    val_indv$stature[i] <- stature
    val_indv$bone[i] <- paste(bone, collapse = ", ")
    val_indv$female[i] <- stature
    val_indv$male[i] <- stature
    val_indv$indet[i] <- stature
    val_indv$n_measures[i] <- n_measures
  }

  if (dim(val_indv)[1] == 0) {
    print("There is no usable bone measurement / indice available for the chosen formula")
  }

  return(val_indv)
}
