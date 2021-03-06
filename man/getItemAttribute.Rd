% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/attributes.R
\name{getItemAttribute}
\alias{getItemAttribute}
\title{Function getItemAttribute}
\usage{
getItemAttribute(dgeObj, attrName)
}
\arguments{
\item{dgeObj}{a DGEobj structre}

\item{attrName}{The name of the item attribute to retrieve}
}
\value{
a list of attribute values for the items
}
\description{
Get a named attribute (key) from all items on a list.
}
\details{
At present, there are no attributes common to all items in a
DGEobj.  This will work on generic lists also where each list
has a common set of attribute keys.
}
\examples{
   #assign attributes to a DGEobj
   MyTypes <- getItemAttribute(dgeObj, "type")
   MyBaseTypes <- getItemAttribute(dgeObj, "basetype")
   MyCreationDates <- getItemAttribute(dgeObj, "dateCreated")

}
\author{
John Thompson, \email{john.thompson@bms.com}
}
\keyword{DGEobj}
\keyword{RNA-Seq,}
