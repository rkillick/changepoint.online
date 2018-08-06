\name{dll<-}
\alias{dll<-}
\title{
Generic Function - dll<-
}
\description{
Generic function
}
\usage{
dll(object)<-value
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
\code{\link{dll<--methods}}
}
\examples{
x=new("ecp.ocpt") # new ocpt object
dll(x)<- c(0) # replaces the existing dll slot with a zero vector.
}

\keyword{methods}
\keyword{ecp.ocpt}
\keyword{internal} 