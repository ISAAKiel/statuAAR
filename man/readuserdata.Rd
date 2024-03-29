% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readuserdata.R
\name{readuserdata}
\alias{readuserdata}
\alias{create.measures.concordance}
\alias{measures.statistics}
\alias{prep.statuaar.data}
\title{statuAAR data preparation and check functions}
\usage{
create.measures.concordance()

measures.statistics(dl)

prep.statuaar.data(
  x,
  d.form = "table",
  ind = NA,
  sex = NA,
  grp = NA,
  measures.names = "own",
  stats = TRUE
)
}
\arguments{
\item{dl}{statuAAR data list as provided by prep.statuaar.data.}

\item{x}{A simple data.frame containing the measurements per individual.}

\item{d.form}{A string defining the data.frame structure.
\itemize{
\item{ d.form=\code{table} for a data.frame with individuals (rows) and measurements (columns).}
\item{ d.form=\code{list}  for a data.frame with at least two columns:
\code{variable}(character, measure name e.g. hum1), \code{value} (numeric, length (mm)).}
}}

\item{ind}{A string defining the column with identifiers for each individual.
If ind = NA a column \code{Ind} with rownumbers will be added.}

\item{sex}{A string defining the column identifying the sex.
If sex = NA a column \code{Sex} with \code{indet} will be added.}

\item{grp}{A string defining a optional grouping variable, e.g. population.
If grp = NA a column \code{Group} with \code{NA} will be added.}

\item{measures.names}{A string defining the set of predefined or own measure names used.
For \code{own} a data.frame \code{measures.concordance} for correlation (merge) is needed.
\itemize{
\item{ measures=\code{short}: Bone (3 letters), measure acc. to Martin (1928),
laterality (1 letter) without any separation
(e.g. Hum1, Hum1l, Hum1r, Hum1a, Hum1al, Hum1ar etc.).}
\item{ measures=\code{long}: Bone measure acc. to Martin (1928), laterality separated by \code{.}
(e.g. Humerus.1, Humerus.1.left, Humerus.1a.left, etc.).}
\item{ measures=\code{own}: A data.frame \code{measures.concordance} with own names to be merged is needed. }
}}

\item{stats}{Output of aggregating statistics of the measures provided. Default = TRUE.}
}
\value{
A list with basic statistics and a dataframe with measures to be processed.

\itemize{
\item \bold{Ind}:  identifyer for each individual.
\item \bold{Sex}: sex of the individual. Accept values 1 (male), 2 (female), 3 (indet) or \code{m}(ale), \code{f}(emale), \code{indet}.
\item \bold{grp}: a grouping variable (e.g. population).
\item \bold{variable}:  short name of the measure
\item \bold{value}: measured value.
}
}
\description{
Human stature estimation is based on various measures of different bones. A multitude of
formula have been developed and provide a wide range of possible results. The quality of
the result depends mainly on the representativity of the original sample of the formula
with respect to the currrent study.  Thus a easy and fast forward calculation according
to various formula can be used for a camparison of the results.

All measures have to be given in millimeters (mm). The measures used are defined by
R. Martin (1928). The labels of the measures of the data aquisition may differ from
those used by statuAAR. Therefore a concordance of labels should be edited.

In addition summarised statistics for each measure across the sample is provided
to check for data inconsitancy befor calculation.

\itemize{
\item{\code{\link{create.measures.concordance}}: Creates a data frame with three columns:
\strong{short} names (e.g. Hum1al), \strong{long} names (e.g. Humerus.1a.left) and
\strong{own} to be filled with user defined names.}
\item{\code{\link{measures.statistics}}: Calculates basic descriptive statistics to check
data consitancy.}
\item{\code{\link{prep.statuaar.data}}: Checks the input data: uniqueness of individual
identifyer, accepted values for Sex and accepted measures names.
Provides a data.frame of standardised measurements
for calculation of body stature estimation with five columns:
\strong{Ind}(ividual), \strong{Sex}, \strong{Group} (optional grouping),
\strong{variable} (mearsure name) and \strong{value} (measured value).}
}
}
\examples{
# Read example dataset into a data frame
x <- TrotterGleser1952
# If not yet existent create a list of measure names to be used
measures.concordance <- create.measures.concordance()
# Edit the measures.list (not needed for this dataset)
measures.concordance$own[measures.concordance$short=="Fem1"]<-"Fem"

# get a dataframe with measures to process
dl.trotter.gleser <- prep.statuaar.data(x, d.form = "table",
   ind = "Appendix_row", sex = "Sex", grp = "Race", measures.names = "own")
# See basic statistics to check for errors
measures.statistics(dl.trotter.gleser)

# For the data from Rollet 1888
rollet1888 <- Rollet1888
# 1. Create an identifyer due to identical numbering of females and males
rollet1888$id<-paste(rollet1888$Sex, rollet1888$Nr, sep="_")
# 2. Fill in the mesasures names in the column "own" of the measures.list
measures.concordance<-read.csv("./data-raw/measures.concordance.rollet1888.csv")
# 3. Read the data
dl.rollet1888 <- prep.statuaar.data(rollet1888, d.form = "table",
      ind="id", sex = "Sex", measures.names = "own")

}
\references{
\insertRef{martinLehrbuchAnthropologieSystematischer1928}{statuAAR}
}
\author{
Christoph Rinne \email{crinne@ufg.uni-kiel.de}

Hendrik Raese \email{h.raese@ufg.uni-kiel.de}
}
