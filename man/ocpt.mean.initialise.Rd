\name{ocpt.mean.initialise}
\alias{ocpt.mean.initialise}
\title{
Identifying Changes in Mean. Initialisation function.
}
\description{
Calculates the optimal positioning and (potentially) number of changepoints for data using the user specified method.
}
\usage{
ocpt.mean.initialise(data,penalty="MBIC",pen.value=0,method="PELT",Q=5,test.stat="Normal",class=TRUE,param.estimates=TRUE,shape=1,minseglen=1)
}
\arguments{
  \item{data}{
	A vector, ts object or matrix containing the data within which you wish to find a changepoint.  If data is a matrix, each row is considered a separate dataset.
}
  \item{penalty}{
	Choice of "None", "SIC", "BIC", "MBIC", AIC", "Hannan-Quinn", "Asymptotic", "Manual" and "CROPS" penalties.  If Manual is specified, the manual penalty is contained in the pen.value parameter. If Asymptotic is specified, the theoretical type I error is contained in the pen.value parameter.  If CROPS is specified, the penalty range is contained in the pen.value parameter; note this is a vector of length 2 which contains the minimum and maximum penalty value.  Note CROPS can only be used if the method is "PELT". The predefined penalties listed DO count the changepoint as a parameter, postfix a 0 e.g."SIC0" to NOT count the changepoint as a parameter.
}
  \item{pen.value}{
	The theoretical type I error e.g.0.05 when using the Asymptotic penalty.  A vector of length 2 (min,max) if using the CROPS penalty.  The value of the penalty when using the Manual penalty option - this can be a numeric value or text giving the formula to use.  Available variables are, n=length of original data, null=null likelihood, alt=alternative likelihood, tau=proposed changepoint, diffparam=difference in number of alternatve and null parameters.  
}
  \item{method}{
	Choice of "AMOC", "PELT", "SegNeigh" or "BinSeg".
}
  \item{Q}{
	The maximum number of changepoints to search for using the "BinSeg" method.  The maximum number of segments (number of changepoints + 1) to search for using the "SegNeigh" method.
}
  \item{test.stat}{
	The assumed test statistic / distribution of the data.  Currently only "Normal" and "CUSUM" supported.
}
  \item{class}{
	Logical.  If TRUE then an object of class \code{cpt} is returned.
}
  \item{param.estimates}{
	Logical.  If TRUE and class=TRUE then parameter estimates are returned. If FALSE or class=FALSE no parameter estimates are returned.
}
\item{shape}{
Only used when cost function is the gamma likelihood.
}
  \item{minseglen}{
  Positive integer giving the minimum segment length (no. of observations between changes), default is the minimum allowed by theory.
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
\code{\link{ocpt.var.initialise}},\code{\link{ocpt.meanvar.initiliase}},\code{\linkS4class{ocpt}}
}
\examples{
# Example of changes in mean at 5, 10 and 3 in simulated normal data
set.seed(1)
x=c(rnorm(50,0,1),rnorm(50,5,1),rnorm(50,10,1),rnorm(50,3,1))
ocpt.mean.initialise(x)
}

\keyword{methods}
\keyword{univar}
\keyword{models}
\keyword{ts}
