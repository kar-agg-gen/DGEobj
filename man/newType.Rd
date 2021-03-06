% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/newType.R
\name{newType}
\alias{newType}
\title{Function newType}
\usage{
newType(dgeObj, itemType, baseType, uniqueItem = FALSE)
}
\arguments{
\item{dgeObj}{A class dgeObj created by function initDGEobj}

\item{itemType}{The name of the new type to create}

\item{baseType}{The basetype of the new item.  One of [row, col, assay, meta]}

\item{uniqueItem}{If set to TRUE, only one instance of the new type is
allowed in a DGEobj}
}
\value{
A DGEobj with a new type definition embedded
}
\description{
Used to customiza a DGEobj definition by adding new types.  A basetype
is also declared and whether more than a single instance is allowed.
}
\examples{
    MyDgeObj <- newType(MyDgeObj,
                        itemType="AffyRMA",
                        baseType="assay",
                        uniqueItem=TRUE)

}
\author{
John Thompson, \email{john.thompson@bms.com}
}
\keyword{DGEobj}
\keyword{RNA-Seq,}
