#' @export
as.statuaar_data_table <- function(x, ...) {

  # define expectations
  necessary_vars <- c("Ind","Sex","Group","variable","value")

  # check if input data type is data.frame or tibble
  if ("data.frame" %in% class(x) | all(c("tbl", "tbl_df") %in% class(x))) {
    # check if necessary vals are present
    present <- necessary_vars %in% colnames(x)
    if (all(present)) {
      # do the actual conversion!
      x %>%
        tibble::new_tibble(., nrow = nrow(.), class = "statuaar_data_table") %>%
        return()
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

@export
as.statuaar_statistics <- function(x, ...) {

  # define expectations
  necessary_vars <- c("measure", "n", "MinM", "Quart1", "MedianM", "MeanM", "Quart3", "MaxM")

  # check if input data type is data.frame or tibble
  if ("data.frame" %in% class(x)) {
    # check if necessary vals are present
    present <- necessary_vars %in% colnames(x)
    if (all(present)) {
      # do the actual conversion!
      x %>%
        tibble::new_tibble(., nrow = nrow(.), class = "statuaar_statistics") %>%
        return()
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

#' @rdname is
#' @export
#' @keywords internal
is.statuaar_data_table<- function(x, ...) {"statuaar_data_table" %in% class(x)}

#' @rdname is
#' @export
#' @keywords internal
is.statuaar_statistics<- function(x, ...) {"statuaar_statistics" %in% class(x)}
