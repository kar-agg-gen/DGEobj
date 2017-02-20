% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/addItem.R
\name{addItems}
\alias{addItems}
\title{Function addItem (DGEobj)}
\usage{
addItems(dgeObj, itemList, overwrite = FALSE, custAttr)
}
\arguments{
\item{dgeObj}{A DGEobj that items will be added to.}

\item{itemList}{A list of data items to add to dgeObj}

\item{overwrite}{Default = FALSE.  Set to TRUE to overwrite the data object
stored in the itemName slot}

\item{custAttr}{A named list of attributes to add to each item (optional)}
}
\value{
A DGEobj class object with new items added.
}
\description{
Add a data item to a class DGEobj
}
\examples{
   #replace a set of contrasts after adding something to each
   myDgeObj <- addItems(myDgeObj, mycontrastList, overwrite=TRUE)

}
\author{
John Thompson, \email{john.thompson@bms.com}
}
\keyword{DGEobj}
\keyword{RNA-Seq,}
