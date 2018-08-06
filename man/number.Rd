\name{number}
\alias{number}
\title{
Generic Function - number
}
\description{
Generic function
}
\usage{
number(object)
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
\code{\link{number-methods}}
}
\examples{
x=new("ecp.ocpt") # new cpt object
number(x) # retrieves the method slot from x
}

\keyword{methods}
\keyword{ecp.ocpt}
\keyword{internal}