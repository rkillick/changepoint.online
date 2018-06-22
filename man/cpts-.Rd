\name{cpts<-}
\alias{cpts<-}
\title{
Generic Function - cpts<-
}
\description{
Generic function
}
\usage{
cpts(object)<-value
}
\arguments{
  \item{object}{
	Depending on the class of \code{object} depends on the method used (and if one exists)
}
  \item{value}{
	Replacement value
}
}
\details{
	Generic function.
}
\value{
	Depends on the class of \code{object}, see individual methods
}
\author{
Andrew Connell, 
Rebecca Killick
}


\seealso{
\code{\link{cpts<--methods}}
}
\examples{
x=new("ocpt") # new ocpt object
}

\keyword{methods}
\keyword{ocpt}
\keyword{internal} 