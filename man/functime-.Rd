\name{functime<-}
\alias{functime<-}
\title{
Generic Function - functime<-
}
\description{
Generic function
}
\usage{
functime(object)<-value
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
\code{\link{functime<--methods}}
}
\examples{
x=new("ecp.ocpt") # new ecp.ocpt object
functime(x)<- c(0) # replaces the existing time slot with zero.
}

\keyword{methods}
\keyword{ecp.ocpt}
\keyword{internal} 