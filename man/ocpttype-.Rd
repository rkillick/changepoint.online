\name{ocpttype<-}
\alias{ocpttype<-}
\title{
Generic Function - ocpttype
}
\description{
Generic function
}
\usage{
ocpttype(object)<-value
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
\code{\link{ocpttype<--methods}}
}
\examples{
x=new("ocpt") # new ocpt object
ocpttype(x)<-"mean" # replaces the existing ocpttype in object x with "mean"
}

\keyword{methods}
\keyword{ocpt}
\keyword{internal}