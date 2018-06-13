\name{nocpts}
\alias{nocpts}
\title{
Generic Function - nocpts
}
\description{
Generic function
}
\usage{
nocpts(object)
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
\code{\link{nocpts-methods}}
}
\examples{
x=new("ocpt") # new cpt object
nocpts(x) # returns the number of changepoints (i.e. length of the ocpts slot in x minus 1)
}

\keyword{methods}
\keyword{ocpt}