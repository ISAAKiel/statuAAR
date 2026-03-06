#' @title Calculate stature estimation according to: Feldesman et al 1990.
#'
#' @name feldesman_etal_1990
#'
#' @description
#' Stature estimation (mm) for Homo erectus (HE), early Neanderthal (EN),
#' Near Eastern Neanderthal (NEN), and early anatomically modern Homo sapiens
#' (EAMHS) based on a regression calculation of one bone measurement,
#' not separated  by sex  (Feldesman et al 1990).
#' Bone measures used: Fem1
#'
#' If bone measures for left and right are provided the mean value will be used,
#' but for statistic information 2 bones will be counted (n_measures).
#' If sex is indet. the mean of male and female stature estimation is given.
#' Based on the measurement of the Femur (Fem1) of different individuals,
#' the stature for males and females is calculated.
#'
#' Returns a data.frame with:
#' \itemize{
#' \item{ ind: individual identifyer (rownames), }
#' \item{ sex: as provided for calculation: m, f, indet.}
#' \item{ stature: estimated on the provided sex and bone measures, }
#' \item{ bone (measure(s)): bones used for calculation, }
#' \item{ if_female (stature): columns with alternative stature for three sex classes, }
#' \item{ if_male (stature), }
#' \item{ if_indet. (stature) and}
#' \item{ n_measures: number of bone measures included:
#'              e.g. 2 Fem1 (left, right)}
#' }
#'
#' @param df data.frame of type statuaar_data_table, containing informations on individual, bone and measurement.
#'
#' @return data.frame with calculated stature and related information per individual.
#'
#' @author Christoph Rinne \email{crinne@@ufg.uni-kiel.de}
#'
#' @references
#'   \insertRef{Feldesman_Kleckner_Lundy_1990}{statuAAR}
#'
#' @examples
#' # Read example dataset into a data frame
#' x <- statuAAR::Feldesman1990
#' # Check for one individual per site
#' table(table(x$Site))
#'
#' # Reduce gender, e.g. M(a), information to m, f.
#' x$Probable.gender <- tolower(gsub("^(\\w).*", "\\1", x$Probable.gender))
# Replace missing Gender or everything else with i(ndet.).
#' x$Probable.gender[nchar(x$Probable.gender) == 0] <- "i"
#'
#' # If not yet existent create a list of measure names to be used
#' measures.concordance <- create.measures.concordance()
#' # Edit the measures.concordance for this dataset
#' measures.concordance$own[measures.concordance$short == "Fem1"] <- "Femur.length"
#'
#' # Prepare tabled data into a long list (statuaar_data_table)
#' dl.fe90 <- statuAAR::prep.statuaar.data(x, d.form = "wide", ind = "Site",
#'               measures.names = "own", sex = "Probable.gender", stats = FALSE)
#'
#' # Calculate stature estimation using this formula.
#' fe90.estimates <- statuAAR::getStature(c("fe90"), dl.fe90)
#'
#' # Extract the corresponding data frame from the returned list object.
#' fe90.estimates[["fe90"]]
#'
#' @export

feldesman_etal_1990 <- function(df){

  df$variable <- gsub("([rl]$)", "", df$variable) # laterality not needed
  # aggregate values for each measure and individual

  # check if needed measures are present
  needed <- getFormulaMeasures('fe90')
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
    df_bones <- subset(df, subset = df$Ind == vec_indv[i])
    # Get measure values needed
    Fem1 <- df_bones$value.mean[df_bones$variable == "Fem1"]
    #Calculate
    stature <- (Fem1 * 100) / 26.74

    stature <- round(stature, 0)

    # write values into data frame of results
    val_indv$sex[i] <- unique(df_bones$Sex)
    val_indv$stature[i] <- stature
    val_indv$bone[i] <- "Fem1"
    val_indv$if_female[i] <- stature
    val_indv$if_male[i] <- stature
    val_indv$if_indet[i] <- stature
    val_indv$n_measures[i] <- df_bones$value.n[df_bones$variable == "Fem1"]
  }

  if (dim(val_indv)[1] == 0) {
    print("There is no usable bone measurement / indice available for the chosen formula")
  }

  return(val_indv)
}
