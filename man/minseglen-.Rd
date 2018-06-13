\name{minseglen<-}
\alias{minseglen<-}
\title{
Generic Function - minseglen<-
}
\description{
Generic function
}
\usage{
minseglen(object)<-value
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
	Generic Function
}
\value{
	Depends on the class of \code{object}, see individual methods
}
\author{
Rebecca Killick
}


\seealso{
\code{\link{minseglen<--methods}}
}
\examples{
x=new("ocpt") # new ocpt object
minseglen(x)<-5 # replaces the existing minseglen slot in x with 5
}

\keyword{methods}
\keyword{ocpt}
\keyword{internal} 