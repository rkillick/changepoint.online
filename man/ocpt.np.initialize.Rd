\name{ocpt.np.initialize} 
\alias{ocpt.np.initialize}
\title{
ocpt.np.initialize - Initialization function for ECP method and test statistic.
}
\description{
Implements the ECP method for identifying changepoints in a given set of summary statistics for a specified delta and alpha.

This function is called by \code{ocpt.mean.initialize}, \code{ocpt.var.initialize}  and \code{ocpt.var.initialize} when \code{test.stat="ECP"}. 
}
\usage{
ocpt.np.initialize(Z,K,delta,alpha,verbose)
}
\arguments{
  \item{Z}{
	A matrix containing the data.
}
  \item{K}{
 The maximum number of changepoints the user wishes to search for.
}
  \item{delta}{
  The window size used to calculate the calculate the complete portion of our approximate test statistic. This also corresponds to one less than the minimum segment size. It is the same as minseglen for PELT.
}
  \item{alpha}{
	The moment index used for determining the distance between and within segments.
}
  \item{verbose}{
  A flag indicating if status updates should be printed.
  }
}
\details{
	This function is used as a wrapper function to implement the ECP algorithm in C++.  It simply creates the necessary worker vectors, ensures all inputs are the correct type, and passes everything to the C function.
}
\value{
	A ecp.ocpt object is returned with a summary:
	\item{number of changepoints}{The estimated number of change points.}
	\item{Estimate Locations}{The location of the change points estimated by the procedure.}
	\item{Goodness of Fit Model}{Goodness of Fit Model's score.}
	\item{Delta}{The window size.}
	\item{Alpha}{The moment index used.}
	\item{Verbose}{A flag indicating if status updates should be printed.}
	\item{Number of Datapoints}{Total number of data pieces observed.}
	\item{Calcualtion Time}{Time procedure took.}
}
\author{
Andrew Connell
Rebecca Killick
David Matteson
}

\examples{
z = matrix(c(rnorm(100,1,5),(rnorm(100,20,1))),ncol=1)
ans = ocpt.np.initialize(z)
ans
}

\keyword{methods}
\keyword{univar}
\keyword{models}
\keyword{ts}
