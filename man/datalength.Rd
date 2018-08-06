\name{datalength}
\alias{datalength}
\title{
Generic Function - datalength
}
\description{
Generic function
}
\usage{
datalength(object)
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
\code{\link{datalength-methods}}
}
\examples{
x=new("ecp.ocpt") # new cpt object
datalength(x) # retrieves the method slot from x
}

\keyword{methods}
\keyword{ecp.ocpt}
\keyword{internal}