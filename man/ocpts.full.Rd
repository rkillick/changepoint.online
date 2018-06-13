\name{ocpts.full}
\alias{ocpts.full}
\title{
Generic Function - ocpts.full
}
\description{
Generic function
}
\usage{
ocpts.full(object)
}
\arguments{
  \item{object}{
	Depending on the class of \code{object} depends on the method used (and if one exists)
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
\code{\link{ocpts.full-methods}}
}
\examples{
x=new("ocpt.range") # new ocpt.range object
ocpts.full(x) # retrieves the ocpts.full slot from x
}

\keyword{methods}
\keyword{ocpt}
\keyword{internal} 