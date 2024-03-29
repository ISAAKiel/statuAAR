% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/telkkae_1950.R
\name{telkkae_1950}
\alias{telkkae_1950}
\title{Calculate stature estimation according to: Telkkä 1950.}
\arguments{
\item{df}{data.frame containing informations on individual, bone and measurement.}
}
\value{
data.frame with calculated stature and related information per individual.
}
\description{
Stature estimation (mm) based on the mean of different regression calculations,
separated  by sex (Citation).
Bone measures used: Hum1, Rad2, Uln2, Fem1, Tib1, Fib1

If bone measures for left and right are provided the mean value will be used,
but for statistic information 2 bones will be counted (n_measures).
If sex is indet. the mean of male and female stature estimation is given.
To retrieve the estimated stature 20 mm will be substracted from the
resulting mean value.

Returns a data.frame with:
\itemize{
\item{ ind: individual identifyer (rownames), }
\item{ sex: as provided for calculation: m, f, indet.}
\item{ stature: estimated on the provided sex and bone measures, }
\item{ bone (measure(s)): bones used for calculation, }
\item{ female (stature): columns with alternative stature for three sex classes, }
\item{ male (stature), }
\item{ indet. (stature) and}
\item{ n_measures: number of bone measures included:
e.g. 2 Fem2 (left, right) + 1 Tib1}
}
}
\examples{

}
\author{
Christoph Rinne \email{crinne@ufg.uni-kiel.de}
}
