\name{time<-}
\alias{time<-}
\title{
Generic Function - time<-
}
\description{
Generic function
}
\usage{
time(object)<-value
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
\code{\link{time<--methods}}
}
\examples{
x=new("ecp.ocpt") # new ecp.ocpt object
time(x)<- c(0) # replaces the existing time slot with zero.
}

\keyword{methods}
\keyword{ecp.ocpt}
\keyword{internal} 