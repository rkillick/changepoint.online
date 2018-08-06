\name{left<-}
\alias{left<-}
\title{
Generic Function - left<-
}
\description{
Generic function
}
\usage{
left(object)<-value
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
\code{\link{left<--methods}}
}
\examples{
x=new("ecp.ocpt") # new ocpt object
left(x)<- matrix(0) # replaces the existing left slot with an empty matrix.
}

\keyword{methods}
\keyword{ecp.ocpt}
\keyword{internal} 