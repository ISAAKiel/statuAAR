#' Bach1965: statuar table example
#'
#' A measurements table from \emph{Bach 1965 tables p. 18, 19}.
#' Data as given in millimeters, but stature converted to millimeters as well.
#' Sex added according to table subscription.
#'
#' @references
#' \insertRef{Bach_1965}{statuAAR}
#'
#' @format A data frame with 64 rows and 7 variables.
#' \itemize{
#'   \item \bold{sex:} f, m -> female, male
#'   \item \bold{Hum1} measures (mm)
#'   \item \bold{Hum2} measures (mm)
#'   \item \bold{Rad1b} measures (mm)
#'   \item \bold{stature} measures (mm)
#'   \item \bold{Fem1} measures (mm)
#'   \item \bold{Tib1b} measures (mm)
#' }
#'
#' @name Bach1965
NULL

#' Feldesman1990: statuar table example
#'
#' A measurements table from \emph{Feldesman et al 1990, Tab. 6}.
#' Raw data, femur/stature estimates, and Trotter-Gleser statuar estimates
#' for sample of mid-to late-pleistocene fossil hominids.
#' (1) Gender according to published criteria:
#' (a) pubic morphology or (b) cranial capacity and skeletal.
#'
#' @references
#' \insertRef{Feldesman_Kleckner_Lundy_1990}{statuAAR}
#'
#' @format A data frame with 48 rows and 7 variables.
#' \itemize{
#'   \item \bold{site} site name abreviation
#'   \item \bold{Type} hominide
#'   \item \bold{Probable.gender: } M(a/b), F(a/b) -> Male, Female
#'   \item \bold{Gender.reference} reference for gender
#'   \item \bold{Femur.length} measures (mm)
#'   \item \bold{Femur.stature} statuar calculation based on Femur (mm)
#'   \item \bold{Trotter-Gleser.stature} statuar calculation based Trotter & Gleser (mm)
#' }
#'
#' @name Feldesman1990
NULL

#' Rollet1888: statuar table example
#'
#' A measurements table from \emph{Rollet 1888, pp. 16-23}.
#' Data as given without calculated columns and stature in millimeters as for the measures.
#' Further explanation mentions a general shrinking of the bones by app. 2 mm within
#' eight to ten months after the first measures in fresh condition.
#' This differentiation is shown in the tables for the femora only. (Rollet 1888 p.24)
#'
#' @references
#' \insertRef{Rollet_1888}{statuAAR}
#'
#' @source \url{https://archive.org/details/BIUSante_57377/page/n17/mode/2up}
#'
#' @format A data frame with 100 rows and 18 variables.
#' \itemize{
#'   \item \bold{Nr} identifier
#'   \item \bold{Sex:} f, m -> female, male
#'   \item \bold{Age} in years
#'   \item \bold{Stature} measures mm
#'   \item \bold{Femur.right} measures mm
#'   \item \bold{Femur.left} measures mm
#'   \item \bold{Femur.right.dry} measures mm
#'   \item \bold{Femur.left.dry} measures mm
#'   \item \bold{Tibia.right} measures mm
#'   \item \bold{Tibia.left} measures mm
#'   \item \bold{Fibula.right} measures mm
#'   \item \bold{Fibula.left} measures mm
#'   \item \bold{Humerus.right} measures mm
#'   \item \bold{Humerus.left} measures mm
#'   \item \bold{Radius.right} measures mm
#'   \item \bold{Radius.left} measures mm
#'   \item \bold{Ulna.right} measures mm
#'   \item \bold{Ulna.left} measures mm
#'  }
#'
#' @name Rollet1888
NULL

#' TrotterGleser1952: statuar table example
#'
#' A measurements table from \emph{Trotter & Gleser 1952, Appendix 1-4}.
#' Data as given but with additional columns related to appendix heading,
#' and stature in millimeters.
#'
#' @references
#' \insertRef{Trotter_Gleser_1952}{statuAAR}
#'
#' @format A data frame with 184 rows and 11 variables.
#' \itemize{
#'   \item \bold{Appendix_row:} identifier
#'   \item \bold{Sex:} f, m -> female, male
#'   \item \bold{race:} white, black
#'   \item \bold{Hum} measures (mm)
#'   \item \bold{Rad} measures (mm)
#'   \item \bold{Ulna} measures (mm)
#'   \item \bold{Fem} measures (mm)
#'   \item \bold{Tib} measures (mm)
#'   \item \bold{Fib} measures (mm)
#'   \item \bold{Fem+Tib} measures (mm)
#' }
#'
#' @name TrotterGleser1952
NULL

#' measures.concordance: concordance for user measure names to
#' list of valid measures according to Martin (1928)
#' @format A data frame with 44 rows and 3 variables.
#' \itemize{
#'   \item \bold{short:} abbreviation, e.g. Fem1l
#'   \item \bold{long:} full name, e.g. Femur.1.left
#'   \item \bold{own:} column for the own measure names
#' }
#'
#' @name measures.concordance
NULL

#' measures.concordance.rollet: concordance for Rollet (1888) measure names to
#' list of valid measures according to Martin (1928)
#' @format A data frame with 44 rows and 3 variables.
#' \itemize{
#'   \item \bold{short:} abbreviation, e.g. Fem1l
#'   \item \bold{long:} full name, e.g. Femur.1.left
#'   \item \bold{own:} column for the own measure names
#' }
#'
#' @name measures.concordance.rollet1888
NULL

#' stuaar_formula: List of implemented formula for stature estimation
#' @format A list with a list for each formula providing three items.
#' \itemize{
#'   \item \bold{short:} character, abbreviation, e.g. bb65.
#'   \item \bold{long:} character, full name used for the function, e.g. breitinger_bach_1965.
#'   \item \bold{measures:} character, vector of short measure names, e.g. Hum1.
#' }
#'
#' @name stuaar_formula
NULL
