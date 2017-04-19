% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/index.R
\name{annotateDGEobj}
\alias{annotateDGEobj}
\title{Function annotateDGEobj}
\usage{
annotateDGEobj(dgeObj, regfile, keys = list("ID", "BMS_PID", "Title",
  "Description", "Keywords", "Business Unit", "Functional Area", "Disease",
  "Vendor", "PlatformType", "Technology", "LibraryPrep", "Organism", "Tissue",
  "AlignmentReference", "GeneModel", "TBio_Owner", "TA_Owner", "LoadData",
  "ReadLength", "ReadType", "Pipeline", "AlignmentAlgorithm", "ScriptID"))
}
\arguments{
\item{dgeObj}{A class dgeObj created by function initDGEobj}

\item{regfile}{An omicsoft registration file (text not Excel!)}

\item{keys}{A list of keys to look for and transfer to the DGEobj}
}
\value{
A DGEobj annotated with attributes.
}
\description{
Read an Omicsoft Registration file and attach attributes to the DGEobj.
}
\details{
Omicsoft registration files contain a [SampleSet] section that contains
metadata about a project.  These are key/value pairs separated by an equals
sign.  The keys parameter specifies which key we want to capture as
attributes on the DGEobj.

As long as you provide a text file with key/value pairs separated by equals
sign, this function should work.
}
\examples{
   MyDgeObj <- annotateDGEobj(DGEobj, regfile)

}
\author{
John Thompson, \email{john.thompson@bms.com}
}
\keyword{DGEobj,}
\keyword{RNA-Seq,}
\keyword{annotation,}
\keyword{attributes}