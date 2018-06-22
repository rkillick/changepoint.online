\name{cpttype<-}
\alias{cpttype<-}
\title{
Generic Function - ocpttype
}
\description{
Generic function
}
\usage{
cpttype(object)<-value
}
\arguments{
  \item{object}{
	Depending on the class of \code{object} depends on the method used (and if one exists)
}
  \item{value}{
	Replacement value
}
}
\details{
	Generic Function
}
\value{
	Depends on the class of \code{object}, see individual methods
}
\author{
Andrew Connell, 
Rebecca Killick
}


\seealso{
\code{\link{cpttype<--methods}}
}
\examples{
x=new("ocpt") # new ocpt object
cpttype(x)<-"mean" # replaces the existing ocpttype in object x with "mean"
}

\keyword{methods}
\keyword{ocpt}
\keyword{internal}