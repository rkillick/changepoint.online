\name{number<-}
\alias{number<-}
\title{
Generic Function - number<-
}
\description{
Generic function
}
\usage{
number(object)<-value
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
\code{\link{number<--methods}}
}
\examples{
x=new("ecp.ocpt") # new ecp.ocpt object
number(x)<- c(0) # replaces the existing number slot with no changepoints.
}

\keyword{methods}
\keyword{ecp.ocpt}
\keyword{internal} 