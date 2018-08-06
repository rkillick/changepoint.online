\name{estimates<-}
\alias{estimates<-}
\title{
Generic Function - estimates<-
}
\description{
Generic function
}
\usage{
estimates(object)<-value
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
Andrew Connell
}


\seealso{
\code{\link{estimates<--methods}}
}
\examples{
x=new("ecp.ocpt") # new ocpt object
estimates(x)<- c(0) # replaces the existing estimates slot with no changepoints/estimates.
}

\keyword{methods}
\keyword{ecp.ocpt}
\keyword{internal} 