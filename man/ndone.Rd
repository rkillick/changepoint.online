\name{ndone}
\alias{ndone}
\title{
Generic Function - ndone
}
\description{
Generic function
}
\usage{
ndone(object)
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
\code{\link{ndone-methods}}
}
\examples{
x=new("ocpt") # new cpt object
ndone(x) # retrieves the method slot from x
}

\keyword{methods}
\keyword{ocpt}
\keyword{internal}