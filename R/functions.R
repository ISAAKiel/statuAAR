#' @name getFormulaNames
#'
#' @title Returns the long names of the formula abbreviation
#'
#' @description
#' Retreaves the long names of the formula abbreviations out of the list with names
#' and bone measures for each formula.
#'
#' @param shortnames A vector of short names.
#'
#' @return Vector with long names of the formula.
#'
#' @author Christoph Rinne \email{crinne@@ufg.uni-kiel.de}
#'
#' @examples
#' # Get the long names of the function abbreviations
#' getFormulaNames(c('bb65', 'tg01'))
#'
#'@export
#'
getFormulaNames <- function (shortnames){
  names <- sapply(statuaar_formula, function(item){
    if (item$short %in% shortnames) {
      return(item$name)
    }
    return(NA) # NA if NULL
  })
  return(as.vector(names[!is.na(names)]))
}

#' @name getFormulaMeasures
#'
#' @title Returns the measures for the formula abbreviation
#'
#' @description
#' Retreaves the bone measures needed out of the list with formula names
#' and bone measures for each formula.
#'
#' @param shortnames A vector of short names.
#'
#' @return Vector with unique bone measure of the formula.
#'
#' @author Christoph Rinne \email{crinne@@ufg.uni-kiel.de}
#'
#' @examples
#' # Get the bone measures needed for the functions.
#' getFormulaMeasures(c('bb65', 'tg01'))
#'
#'@export
#'
getFormulaMeasures <- function (shortnames){
  measures <- sapply(statuaar_formula, function(item){
    if (item$short %in% shortnames) {
      return(item$measures)
    }
    return(NA) # NA if NULL
  })
  return(unique(unlist(measures[!is.na(measures)])))
}

#' @name getFormulaDataframe
#'
#' @title Returns a data.frame containing formula informations.
#'
#' @description
#' Returns a data.frame with three columns:
#'
#'  \itemize{
#'    \item{ short: short name for the formula, e.g. bb65, tg01. }
#'    \item{ long: long name of the function name, e. g. breitinger_bach:1965,
#'    trotter_gleser_1952_an. }
#'    \item{ measures: vector of measure names (Martin 1928), e.g. Hum1, Fem1, Tib1b }
#'  }
#'
#' @return Data.frame with columns.
#'
#' @author Christoph Rinne \email{crinne@@ufg.uni-kiel.de}
#'
#' @examples
#' # Get the bone measures needed for the functions.
#' getFormulaDataframe()
#'
#'@export
#'
getFormulaDataframe <- function(){
  tmp_list <- list()
  for (element in statuaar_formula) {
    short <- element[[1]]
    long <- element[[2]]
    measures <- element[[3]]
    tmp_list <- append(tmp_list, list(c(short, long, paste(measures, collapse = ', '))))
  }
  tmp_df <- do.call(rbind, tmp_list)
  tmp_df <- as.data.frame(tmp_df, stringsAsFactors = FALSE)
  colnames(tmp_df) <- c('short', 'long', 'measures')
  print(tmp_df)
}

#' @name getStature
#'
#' @title Returns a list with data.frames with stature estimations.
#'
#' @description
#' Returns a list with data.frames of stature estimations for each specified formula
#'  and the data provided in a data.frame of class statuaar_data_table.
#'
#' @param shortnames A vector of short names of the formula, e.g. c('bb65', 'tg01').
#'
#' @param statuaar_data_table A data.frame of class statuaar_data_table as provided
#' by prep.statuaar.data().
#' #' \itemize{
#'   \item \bold{Ind}:  identifyer for each individual.
#'   \item \bold{Sex}: sex of the individual. Accepted values:
#'   1 (male), 2 (female), 3 (indet) or `m`(ale), `f`(emale), `indet`.
#'   \item \bold{variable}:  short name of the measure
#'   \item \bold{value}: measured value.
#' }
#'
#' @return A list with a data.frame for each specified stature estimation formula.
#' The item name corresponds to the specified formula.
#' Each data.frame with seven columns:
#'  \itemize{
#'    \item{ \bold{ind:} individual identifyer (rownames), }
#'    \item{ \bold{sex:} as provided for calculation: m, f, indet.}
#'    \item{ \bold{stature:} estimated on the provided sex and bone measures, }
#'    \item{ \bold{bone measure name(s)}: bones used for calculation, }
#'    \item{ \bold{female stature}: columns with alternative stature for three sex classes, }
#'    \item{ \bold{male stature}, }
#'    \item{ \bold{indet. stature} and}
#'    \item{ \bold{n_measures}: number of bone measures included:
#'              e.g. 2 Fem2 (left, right) + 1 Tib1}
#'  }
#'
#' @author Christoph Rinne \email{crinne@@ufg.uni-kiel.de}
#'
#' @examples
#' # Get the bone measures needed for the functions.
#' getStature(c('bb65', 'tg01'), dl.trotter.gleser)
#'
#'@export
#'
getStature <- function(shortnames, statuaar_data_table) {
  # List for the data.frames with stature estimations per formula.
  stature_list <- list()

  for (shortname in shortnames) {
    # Find function (item) in the statuaar_formula list
    item <- Filter(function(x) x$short == shortname, statuaar_formula)

    # Wenn das Item gefunden wurde, rufe die Funktion auf
    if (length(item) > 0) {
      func_name <- item[[1]]$name
      func <- get(func_name)  # Holt die Funktion anhand des Namens
      df_result <- func(statuaar_data_table)  # Führt die Funktion aus und erhält den Data Frame
      stature_list[[shortname]] <- df_result  # Speichert den Data Frame in der Liste
    } else {
      stature_list[[shortname]] <- NULL  # Falls kein Treffer, NULL speichern
    }
  }

  return(stature_list)
}
