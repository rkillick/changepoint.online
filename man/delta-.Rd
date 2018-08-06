\name{delta<-}
\alias{delta<-}
\title{
Generic Function - delta<-
}
\description{
Generic function
}
\usage{
delta(object)<-value
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
\code{\link{delta<--methods}}
}
\examples{
x=new("ecp.ocpt") # new ocpt object
delta(x)<- c(0) # replaces the existing delta slot with a new delta value for window size.
}

\keyword{methods}
\keyword{ecp.ocpt}
\keyword{internal} 