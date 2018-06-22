\name{cost_func<-}
\alias{cost_func<-}
\title{
Generic Function - cost_func<-
}
\description{
Generic function
}
\usage{
cost_func(object)<-value
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
Andrew Connell
}


\seealso{
\code{\link{cost_func<--methods}}
}
\examples{
x=new("ocpt") # new ocpt object
cost_func(x)<- "mean.norm" # replaces the existing method slot in x with "mean"
}

\keyword{methods}
\keyword{ocpt}
\keyword{internal} 