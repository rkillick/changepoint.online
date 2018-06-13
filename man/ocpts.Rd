\name{ocpts}
\alias{ocpts}
\title{
Generic Function - ocpts
}
\description{
Generic function
}
\usage{
ocpts(object)
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
\code{\link{ocpts-methods}}
}
\examples{
x=new("ocpt") # new ocpt object
ocpts(x) # retrieves the ocpts slot from x
}

\keyword{methods}
\keyword{ocpt}
\keyword{internal}