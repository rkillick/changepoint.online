\name{ndone<-}
\alias{ndone<-}
\title{
Generic Function - ndone<-
}
\description{
Generic function
}
\usage{
ndone(object)<-value
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
\code{\link{ndone<--methods}}
}
\examples{
x=new("ocpt") # new ocpt object
ndone(x)<- c(0) # replaces the existing method slot in x with "mean"
}

\keyword{methods}
\keyword{ocpt}
\keyword{internal} 