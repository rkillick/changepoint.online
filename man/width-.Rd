\name{width<-}
\alias{width<-}
\title{
Generic Function - width<-
}
\description{
Generic function
}
\usage{
width(object)<-value
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
\code{\link{width<--methods}}
}
\examples{
x=new("ecp.ocpt") # new ocpt object
width(x)<- c(1) # replaces the existing width slot with a matrix column width of 1.
}

\keyword{methods}
\keyword{ecp.ocpt}
\keyword{internal} 