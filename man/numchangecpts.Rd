\name{numchangecpts}
\alias{numchangecpts}
\title{
Generic Function - numchangecpts
}
\description{
Generic function
}
\usage{
numchangecpts(object)
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
\code{\link{numchangecpts-methods}}
}
\examples{
x=new("ocpt") # new cpt object
numchangecpts(x) # retrieves the method slot from x
}

\keyword{methods}
\keyword{ocpt}
\keyword{internal}