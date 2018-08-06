\name{cpLoc<-}
\alias{cpLoc<-}
\title{
Generic Function - cpLoc<-
}
\description{
Generic function
}
\usage{
cpLoc(object)<-value
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
\code{\link{cpLoc<--methods}}
}
\examples{
x=new("ecp.ocpt") # new ocpt object
cpLoc(x)<- list(0) # replaces the existing changepoint location slot with no changepoints.
}

\keyword{methods}
\keyword{ecp.ocpt}
\keyword{internal} 