\name{drr<-}
\alias{drr<-}
\title{
Generic Function - drr<-
}
\description{
Generic function
}
\usage{
drr(object)<-value
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
\code{\link{drr<--methods}}
}
\examples{
x=new("ecp.ocpt") # new ocpt object
drr(x)<- c(0) # replaces the existing drr slot with zero vector.
}

\keyword{methods}
\keyword{ecp.ocpt}
\keyword{internal} 