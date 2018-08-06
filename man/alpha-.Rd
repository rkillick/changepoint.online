\name{alpha<-}
\alias{alpha<-}
\title{
Generic Function - alpha<-
}
\description{
Generic function
}
\usage{
alpha(object)<-value
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
\code{\link{alpha<--methods}}
}
\examples{
x=new("ecp.ocpt") # new ocpt object
alpha(x)<- c(0) # replaces the existing alpha slot with a moment index of zero.
}

\keyword{methods}
\keyword{ecp.ocpt}
\keyword{internal} 