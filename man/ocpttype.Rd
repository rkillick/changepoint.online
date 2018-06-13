\name{ocpttype}
\alias{ocpttype}
\title{
Generic Function - ocpttype
}
\description{
Generic function
}
\usage{
ocpttype(object)
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
Rebecca Killick
}


\seealso{
\code{\link{ocpttype-methods}}
}
\examples{
x=new("ocpt") # new ocpt object
ocpttype(x) # retrieves the ocpttype slot from x
}

\keyword{methods}
\keyword{ocpt}
\keyword{internal} 