% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/as.R
\name{convertRSE}
\alias{convertRSE}
\title{Function convertRSE}
\usage{
convertRSE(RSE, Class, countsName = "Counts")
}
\arguments{
\item{RSE}{A class RangedSummarizedExperiment}

\item{Class}{Destination class (one of "DGEobj", "ES", "ExpressionSet")}

\item{countsName}{If the name of the counts object is not "Counts", use
this argument to tell the function the actual name of the counts assay in the RSE.}
}
\value{
A obejct of the specified class
}
\description{
Casts an RSE class object as a DGEobj.  Only captures count
data and the associated row and col annotation
}
\examples{
   MyDGEobj <- convertRSE(MyRSE, "DGEobj")
   MyES <- convertRSE(MyRSE, "ES")

}
\author{
John Thompson, \email{john.thompson@bms.com}
}
\keyword{DGEobj,}
\keyword{ExpressionSet}
\keyword{RNA-Seq,}
\keyword{SummarizedExperiment,}
