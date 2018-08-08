\name{functime}
\alias{functime}
\title{
Generic Function - functime
}
\description{
Generic function
}
\usage{
functime(object)
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
\code{\link{functime-methods}}
}
\examples{
x=new("ecp.ocpt") # new cpt object
functime(x) # retrieves the method slot from x
}

\keyword{methods}
\keyword{ecp.ocpt}
\keyword{internal}