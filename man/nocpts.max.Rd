\name{nocpts.max}
\alias{nocpts.max}
\title{
Generic Function - nocpts.max
}
\description{
Generic function
}
\usage{
nocpts.max(object)
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
\code{\link{nocpts.max-methods}}
}
\examples{
x=new("ocpt") # new ocpt object
nocpts.max(x) # retrieves the nocpts.max slot from x
}

\keyword{methods}
\keyword{ocpt}
\keyword{internal}