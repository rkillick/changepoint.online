\name{ocpt.var.update}
\alias{ocpt.var.update}
\title{
Identifying Changes in Mean and variance. Update function.
}
\description{
Calculates the optimal positioning and (potentially) number of changepoints for data using the user specified method. The update function should be used once \code{ocpt.var.initialise} has created an answer. 
}
\usage{
ocpt.var.update(previousanswer, newdata)
}
\arguments{
  \item{previousanswer}{
	A S4 class. This will be the output from the \code{ocpt.mean.initialise} function. 
}
 \item{newdata}{
	A vector, ts object or matrix containing the new data within which you wish to find a changepoint.  If data is a matrix, each row is considered a separate dataset.
} 
}
\details{
	This function is used to find changes in mean for data using the test statistic specified in the test.stat parameter.  The changes are found using the method supplied which can be single changepoint or multiple changepoints.  A changepoint is denoted as the last observation of the segment / regime.
}
\value{
  If \code{class=TRUE} then an object of S4 class "ocpt" is returned.  The slot \code{ocpts} contains the changepoints that are returned.  For \code{class=FALSE} the structure is as follows.

  If data is a vector (single dataset) then a vector/list is returned depending on the value of method.  If data is a matrix (multiple datasets) then a list is returned where each element in the list is either a vector or list depending on the value of method.

	Returns a summary of:
	\item{cpttype}{In this case it will always be change in mean.}
	\item{Method}{The method used.}
	\item{test.stat}{The chosen test statistic.}
	\item{penalty}{Both the type and value of the penalty}
	\item{minseglen}{The minimum segment length.}
	\item{max no. of cpts}{Maximum number of changepoints possible.}
	\item{ndone}{Length of the time series when analysis begins.}
	\item{nupdate}{Length of the time series to be analysed in this update.}
}
\references{
Change in Normal mean: Hinkley, D. V. (1970) Inference About the Change-Point in a Sequence of Random Variables, \emph{Biometrika} \bold{57}, 1--17

PELT Algorithm: Killick R, Fearnhead P, Eckley IA (2012) Optimal detection of changepoints with a linear computational cost, \emph{JASA} \bold{107(500)}, 1590--1598

MBIC: Zhang, N. R. and Siegmund, D. O. (2007) A Modified Bayes Information Criterion with Applications to the Analysis of Comparative Genomic Hybridization Data. \emph{Biometrics} \bold{63}, 22-32.
}
\author{
Andrew Connell
Rebecca Killick
}


\seealso{
\code{\link{ocpt.mean.initialise}},\code{\link{ocpt.meanvar.initiliase}},\code{\linkS4class{ocpt}}
}
\examples{
# Example of changes in mean at 5, 10 and 3 in simulated normal data
set.seed(1)
x=c(rnorm(50,0,1),rnorm(50,5,1),rnorm(50,10,1),rnorm(50,3,1))
y = c(rnorm(50,15,1),rnorm(50,25,1),rnorm(50,33,1),rnorm(50,7,1))
previousans = ocpt.var.initialise(x)
ocpt.var.update(previousanswer = previousans, newdata = y)
}

\keyword{methods}
\keyword{univar}
\keyword{models}
\keyword{ts}
