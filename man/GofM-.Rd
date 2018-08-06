\name{GofM<-}
\alias{GofM<-}
\title{
Generic Function - GofM<-
}
\description{
Generic function
}
\usage{
GofM(object)<-value
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
\code{\link{GofM<--methods}}
}
\examples{
x=new("ecp.ocpt") # new ocpt object
GofM(x)<- c(0) # replaces the existing Goodness of fit slot with a goodness of fit score of zero.
}

\keyword{methods}
\keyword{ecp.ocpt}
\keyword{internal} 