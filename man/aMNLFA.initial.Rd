% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aMNLFA_initial.R
\name{aMNLFA.initial}
\alias{aMNLFA.initial}
\title{aMNLFA initial model fitting function}
\usage{
aMNLFA.initial(input.object)
}
\arguments{
\item{input.object}{The aMNLFA object (created using the aMNLFA.object function) which provides instructions for the function.}
}
\value{
No return value. Generates .INP files to test mean and variance impact, as well as DIF for all items, in \emph{Mplus}, in the directory specified in the aMNLFA.object.
}
\description{
This function generates the initial itemwise aMNLFA models.
}
\examples{
 wd <- tempdir()
 first<-paste0(system.file(package='aMNLFA'),"/extdata")
 the.list <- list.files(first,full.names=TRUE)
 file.copy(the.list,wd,overwrite=TRUE)
 ob <- aMNLFA::aMNLFA.object(dir = wd, 
 mrdata = xstudy, 
 indicators = paste0("BIN_", 1:12),
 catindicators = paste0("BIN_", 1:12), 
 meanimpact = c("AGE", "GENDER", "STUDY"), 
 varimpact = c("AGE", "GENDER", "STUDY"), 
 measinvar = c("AGE", "GENDER", "STUDY"),
 factors = c("GENDER", "STUDY"),
 ID = "ID",
 thresholds = FALSE)
 
 aMNLFA.initial(ob)
 
}
\keyword{MNLFA}
