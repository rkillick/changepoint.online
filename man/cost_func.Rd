\name{cost_func}
\alias{cost_func}
\title{
Generic Function - cost_func
}
\description{
Generic function
}
\usage{
cost_func(object)
}
\arguments{
  \item{object}{
	Depending on the class of \code{object} depends on the cost_func used (and if one exists)
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
\code{\link{cost_func-methods}}
}
\examples{
x=new("ocpt") # new cpt object
cost_func(x) # retrieves the method slot from x
}

\keyword{methods}
\keyword{ocpt}
\keyword{internal}