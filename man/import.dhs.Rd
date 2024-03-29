% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import.dhs.prevR.r
\name{import.dhs}
\alias{import.dhs}
\title{Import DHS data.}
\usage{
import.dhs(file.sav, file.dbf)
}
\arguments{
\item{file.sav}{DHS data (one individual per line) in SPSS format (.sav),
downloaded from \url{https://www.dhsprogram.com/}. Could also be directly a
data.frame.}

\item{file.dbf}{GPS position of clusters in DATABASE format (.dbf),
downloaded from \url{https://www.dhsprogram.com/}.
Could also be directly a data.frame.}
}
\description{
This step by step function guides users to import data from a Demographic
and Health Survey (DHS)
and create an object of class \linkS4class{prevR}.
}
\note{
If you don't provide the precise path of files, \R will check the
working directory (see \code{\link[base:getwd]{base::setwd()}}).
To specify the file path, see \code{\link[base:file.path]{base::file.path()}}.

This function was developed specifically for importing DHS.
For a generic function for creating an object of class \linkS4class{prevR},
see \code{\link[=as.prevR]{as.prevR()}}.
}
\examples{
\dontrun{
imported_data <- import.dhs("data.sav", "gps.dbf")
}

}
\seealso{
\code{\link[=as.prevR]{as.prevR()}}, \linkS4class{prevR} class.
}
\keyword{manip}
