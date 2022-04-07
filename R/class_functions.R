#' {mortaar_life_table} and {mortaar_life_table_list}
#'
#' @description The \strong{mortaar_life_table} is the central data structure of the
#' \code{mortAAR} package. It's a data.frame with set of custom methods and
#' variables. Please see \code{mortAAR::life.table} for a description
#' of the variables. Further available variables are ignored. \cr
#' If an object is of class data.frame or tibble (tbl & tbl_df), it can be
#' converted to an object of class mortaar_life_table. The only requirement
#' is that it contains at least the essential columns \strong{a} and \strong{Dx}.
#' The \code{as} function adds the string "mortaar_life_table" to the classes vector. \cr
#' The \strong{mortaar_life_table_list} is a list of mortaar_life_tables.
#' It can contain the additional attribute \code{group} that stores a string with
#' the name of the grouping variable relevant for the separation of the
#' different mortaar_life_tables in the list. The group variable is only relevant
#' for plot and print aesthetics.
#'
#' @param x an object
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' # a mortaar_life_table can be put together manually:
#' as.mortaar_life_table(data.frame(a = c(20, 20, 20), Dx = c(10, 15, 20)))
#'
#' # a mortaar_life_table_list can be constructed from multiple mortaar_life_tables
#' schleswig <- as.mortaar_life_table_list(
#'   list(
#'     "schleswig data 1" = life.table(schleswig_ma[c("a", "Dx")]),
#'     "schleswig data 2" = life.table(schleswig_ma[c("a", "Dx")])
#'   )
#' )
#'
#' # you can add new mortaar_life_tables to plot them with the others
#' schleswig$`schleswig data 3` <- life.table(schleswig_ma[c("a", "Dx")])
#' schleswig[["schleswig data 4"]] <- life.table(schleswig_ma[c("a", "Dx")])
#'
#' # and you can create arbitrary subsets of mortaar_life_table_lists
#' schleswig_data_3 <- schleswig$`schleswig data 3`
#' schleswig_data_1_3_4 <- schleswig[c(1,3,4)]
#'
#' @name mortaar_life_table
NULL

#' @rdname mortaar_life_table
#' @export
as.statuaar_input_list <- function(x, ...) {

  # check input data type is list
  if ("list" %in% class(x)) {
    # check if all elements in list are mortaar_life_table
    if (inherits(x[1], "statuaar_statistics") && inherits(x[2], "statuaar_data_table")) {
      # do the actual conversion!
      x %>%
        `class<-`(c("statuaar_input_list", class(.))) %>%
        return()
    }else {
      stop(
        "One or more elements of x are not of class statuaar_input_list."
      )
    }
  } else {
    stop("x is not an object of class list")
  }
}
NULL
#' @rdname statuaar_statistics
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

#' @rdname statuaar_data_table
#' @export
as.statuaar_input_table <- function(x, ...) {

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



#' @name is
#'
#' @title Checks if a variable is of class mortaar_life_table or mortaar_life_table_list
#'
#' @param x a variable.
#' @param ... further arguments passed to or from other methods.
#'
#' @return true if x is a mortaar_life_table or a mortaar_life_table_list, otherwise false.
#'
#' @examples
#' # Create a mortaar_life_table from a prepared dataset.
#' class(schleswig_ma)
#' is.mortaar_life_table(schleswig_ma)
#'
#' schleswig_1 <- life.table(schleswig_ma[c("a", "Dx")])
#'
#' class(schleswig_1)
#' is.mortaar_life_table(schleswig_1)
#'
#' # Create a mortaar_life_table_list from two datasets.
#' odagsen <- life.table(list(
#'   "corpus mandibulae" = odagsen_cm[c("a", "Dx")],
#'   "margo orbitalis" = odagsen_mo[c("a", "Dx")]
#' ))
#' is.mortaar_life_table_list(odagsen)
#'
#' @rdname is
#' @keywords internal
NULL

#' @rdname is
#' @export
#' @keywords internal
is.statuaar_input_list<- function(x, ...) {"statuaar_input_list" %in% class(x)}

#' @rdname is
#' @export
#' @keywords internal
is.statuaar_data_table<- function(x, ...) {"statuaar_data_table" %in% class(x)}

#' @rdname is
#' @export
#' @keywords internal
is.statuaar_statistics<- function(x, ...) {"statuaar_statistics" %in% class(x)}
