\name{ocpt.np.update}
\alias{ocpt.np.update}
\title{
Identifying Changes Via ECP. Update function.
}
\description{
Calculates the optimal positioning and (potentially) number of changepoints for data using the user specified method. The update function should be used once \code{e.cp3o_delta.online.initialise} has created an answer. 
}
\usage{
ocpt.np.update(previousanswer, newdata,K)
}
\arguments{
  \item{previousanswer}{
	A S4 class. This will be the output from the \code{e.cp3o_delta.online.initialise} function. 
}
 \item{newdata}{
	A vector, ts object or matrix containing the new data within which you wish to find a changepoint.  If data is a matrix, each row is considered a separate dataset.
} 
\item{K}{
Maximum number of changpoints the algorithm will output as estimates. Also the total number of iterations the dynamic programming will do.
}
}
\details{
	This function is used to find changes in mean and variance for data.  The changes are found using the method supplied which can be a single changepoint or multiple changepoints.  A changepoint is denoted as the last observation of the segment / regime.
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
Andrew Connell,
Rebecca Killick,
David Matteson
}


\seealso{
\code{\link{ocpt.var.initialise}},\code{\link{ocpt.meanvar.initialise}},\code{\linkS4class{ocpt}}
}
\examples{
set.seed(1)
x = matrix(c(rnorm(100,50,1),rnorm(100,5,1)),ncol=1)
y=matrix(c(rnorm(100,15,1),rnorm(100,25,1)),ncol=1)
ans = ocpt.np.initialise(x)
updatedans = ocpt.np.update(ans,y,K=3)
updatedans
}

\keyword{methods}
\keyword{univar}
\keyword{models}
\keyword{ts}
