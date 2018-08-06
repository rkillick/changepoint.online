\name{dlr<-}
\alias{dlr<-}
\title{
Generic Function - dlr<-
}
\description{
Generic function
}
\usage{
dlr(object)<-value
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
\code{\link{dlr<--methods}}
}
\examples{
x=new("ecp.ocpt") # new ocpt object
dlr(x)<- c(0) # replaces the existing dlr slot with zero vector.
}

\keyword{methods}
\keyword{ecp.ocpt}
\keyword{internal} 