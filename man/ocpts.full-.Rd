\name{ocpts.full<-}
\alias{ocpts.full<-}
\title{
Generic Function - ocpts.full<-
}
\description{
Generic function
}
\usage{
ocpts.full(object)<-value
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
	Generic function.
}
\value{
	Depends on the class of \code{object}, see individual methods
}
\author{
Rebecca Killick
}


\seealso{
\code{\link{ocpts.full<--methods}}
}
\examples{
x=new("ocpt.range") # new cpt.range object
ocpts.full(x)<-matrix(c(10,20,10,NA),nrow=2,byrow=TRUE) 
# replaces the vector of changepoint in object x with the given matrix
}

\keyword{methods}
\keyword{cpt}
\keyword{internal} 