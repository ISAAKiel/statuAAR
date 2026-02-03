#' @name statuaar_class_functions
#'
#' @title checks and sets statuaar objects
#'
#' @description
#' The statuaar_data_table is the central data structure for stature estimation
#' in \code{statuAAR} package. It is a data.frame with specific variables,
#' s. \code{statuAAR::prep.statuaar.data}.
#'
#'

#' @rdname statuaar_class_functions
#' @export
as.statuaar_data_table <- function(x, ...) {

  # define expectations
  necessary_vars <- c("Ind","Sex","variable","value")

  # check if input data type is data.frame or tibble
  if ("data.frame" %in% class(x) | all(c("tbl", "tbl_df") %in% class(x))) {
    # check if necessary vals are present
    present <- necessary_vars %in% colnames(x)
    if (all(present)) {
      # do the actual conversion!
      (function(y) {
        tibble::new_tibble(x = y, nrow = nrow(y), class = "statuaar_data_table")
      })(x)
    } else {
      stop(
        "The following variables (columns) are missing: ",
        paste(necessary_vars[!present], collapse = ", ")
      )
    }
  } else {
    stop("x is not an object of class data.frame or tibble")
  }

}

#' @rdname statuaar_class_functions
#' @export
as.statuaar_statistics <- function(x, ...) {

  # define expectations
  necessary_vars <- c("measure", "n", "MinM", "Quart1", "MedianM", "MeanM", "Quart3", "MaxM")

  # check if input data type is data.frame or tibble
  if ("data.frame" %in% class(x)) {
    # check if necessary vals are present
    present <- necessary_vars %in% colnames(x)
    if (all(present)) {
      # do the actual conversion!
      (function(y) {
        tibble::new_tibble(x = y, nrow = nrow(y), class = "statuaar_statistics")
      })(x)
    } else {
      stop(
        "The following variables (columns) are missing: ",
        paste(necessary_vars[!present], collapse = ", ")
      )
    }
  } else {
    stop("x is not an object of class data.frame or tibble")
  }

}

#' @rdname statuaar_class_functions
#' @export
#' @keywords internal
is.statuaar_data_table<- function(x, ...) {"statuaar_data_table" %in% class(x)}

#' @rdname statuaar_class_functions
#' @export
#' @keywords internal
is.statuaar_statistics<- function(x, ...) {"statuaar_statistics" %in% class(x)}
