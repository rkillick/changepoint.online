\name{datalength<-}
\alias{datalength<-}
\title{
Generic Function - datalength<-
}
\description{
Generic function
}
\usage{
datalength(object)<-value
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
\code{\link{datalength<--methods}}
}
\examples{
x=new("ecp.ocpt") # new ocpt object
datalength(x)<- c(10) # replaces the existing data length
# slot with data length of 10. In other words, 10 pieces of data.
}

\keyword{methods}
\keyword{ecp.ocpt}
\keyword{internal} 