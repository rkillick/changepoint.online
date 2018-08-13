\name{sumstat<-}
\alias{sumstat<-}
\title{
Generic Function - sumstat<-
}
\description{
Generic function
}
\usage{
sumstat(object)<-value
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
Rebecca Killick
}


\seealso{
\code{\link{sumstat<--methods}}
}
\examples{
x=new("ocpt") # new ocpt object
sumstat(x)<-matrix(c(1,2,3,4,5)) # replaces the existing sumstat slot in x with matrix(1,2,3,4,5)
}

\keyword{methods}
\keyword{ocpt}
\keyword{internal} 