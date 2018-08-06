\name{cpLoc}
\alias{cpLoc}
\title{
Generic Function - cpLoc
}
\description{
Generic function
}
\usage{
cpLoc(object)
}
\arguments{
  \item{object}{
	Depending on the class of \code{object} depends on the method used (and if one exists)
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
\code{\link{cpLoc-methods}}
}
\examples{
x=new("ecp.ocpt") # new cpt object
cpLoc(x) # retrieves the method slot from x
}

\keyword{methods}
\keyword{ecp.ocpt}
\keyword{internal}