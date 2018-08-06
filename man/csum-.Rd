\name{csum<-}
\alias{csum<-}
\title{
Generic Function - csum<-
}
\description{
Generic function
}
\usage{
csum(object)<-value
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
\code{\link{csum<--methods}}
}
\examples{
x=new("ecp.ocpt") # new ecp.ocpt object
csum(x)<- c(0) # replaces the existing csum slot with no cumulative sum.
}

\keyword{methods}
\keyword{ecp.ocpt}
\keyword{internal} 