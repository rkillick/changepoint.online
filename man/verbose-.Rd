\name{verbose<-}
\alias{verbose<-}
\title{
Generic Function - verbose<-
}
\description{
Generic function
}
\usage{
verbose(object)<-value
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
\code{\link{verbose<--methods}}
}
\examples{
x=new("ecp.ocpt") # new ocpt object
verbose(x)<- TRUE # replaces the existing verbose slot with a logical.
}

\keyword{methods}
\keyword{ecp.ocpt}
\keyword{internal} 