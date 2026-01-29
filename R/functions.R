# Function returns formula (long) names.
# Input: vector of short names, c('bb65', 'tg01')
getFormulaNames <- function (shortnames){
  names <- sapply(statuaar_formula, function(item){
    if (item$short %in% shortnames) {
      return(item$name)
    }
    return(NA) # NA if NULL
  })
  return(as.vector(names[!is.na(names)]))
}

# Function returns all measures used in the formula.
# Input: vector of short names, c('bb65', 'tg01')
getFormulaMeasures <- function (shortnames){
  measures <- sapply(statuaar_formula, function(item){
    if (item$short %in% shortnames) {
      return(item$measures)
    }
    return(NA) # NA if NULL
  })
  return(unique(unlist(measures[!is.na(measures)])))
}

# Function returns the list statuaar_formular as data.fram with three columns.
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

# Function applies a list of short formula names on the measures.
# Input:
# - vector of short names, c('bb65', 'tg01')
# - a statuaar_data_table
# Return: List for the data.frames with stature estimations per formula.
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
