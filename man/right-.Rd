\name{right<-}
\alias{right<-}
\title{
Generic Function - right<-
}
\description{
Generic function
}
\usage{
right(object)<-value
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
\code{\link{right<--methods}}
}
\examples{
x=new("ecp.ocpt") # new ocpt object
right(x)<- matrix(0) # replaces the existing right slot with an empty matrix.
}

\keyword{methods}
\keyword{ecp.ocpt}
\keyword{internal} 