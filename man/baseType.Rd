% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get.R
\name{baseType}
\alias{baseType}
\title{Function BaseType}
\usage{
baseType(dgeObj, type)
}
\arguments{
\item{dgeObj}{A class DGEobj created by function initDGEobj}

\item{type}{A type for which you want the basetype}
}
\value{
A basetype value (chracter string)
}
\description{
Return the basetype for a given type
}
\examples{
   MyBaseType <- baseType(dgeObj, type="DGEList")

}
\author{
John Thompson, \email{john.thompson@bms.com}
}
\keyword{DGEobj}
\keyword{RNA-Seq,}
