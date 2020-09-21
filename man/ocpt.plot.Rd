\name{ocpt.plot}
\alias{ocpt.plot}
\title{
Plotting changes in Mean and variance. Update function.
}
\description{
Plots the optimal positioning of changepoints for data using the user specified method. 
}
\usage{
ocpt.plot(data,type = "mean", penalty="Manual", pen.value=length(data),
Q=5, test.stat="Normal", class=TRUE, param.estimates=TRUE, shape=1,
tol = 1e-07, know.mean = FALSE, mu = NA, minseglen=2)
}
\arguments{
  \item{data}{
	The data that the user wants to plot. 
}
\item{type}{
	The type of change the user wants the function to search for. This can be "mean", "var" or "meanvar".  
}
 \item{penalty}{
	Choice of "None", "SIC", "BIC", "MBIC", AIC", "Hannan-Quinn", "Asymptotic", "Manual" and "CROPS" penalties.  If Manual is specified, the manual penalty is contained in the pen.value parameter. If Asymptotic is specified, the theoretical type I error is contained in the pen.value parameter.  If CROPS is specified, the penalty range is contained in the pen.value parameter; note this is a vector of length 2 which contains the minimum and maximum penalty value.  Note CROPS can only be used if the method is "PELT". The predefined penalties listed DO count the changepoint as a parameter, postfix a 0 e.g."SIC0" to NOT count the changepoint as a parameter.
}
  \item{pen.value}{
	The theoretical type I error e.g.0.05 when using the Asymptotic penalty.  A vector of length 2 (min,max) if using the CROPS penalty.  The value of the penalty when using the Manual penalty option - this can be a numeric value or text giving the formula to use.  Available variables are, n=length of original data, null=null likelihood, alt=alternative likelihood, tau=proposed changepoint, diffparam=difference in number of alternatve and null parameters.  
}
  \item{Q}{
	The maximum number of changepoints to search for using the "BinSeg" method.  The maximum number of segments (number of changepoints + 1) to search for using the "SegNeigh" method.
}
  \item{test.stat}{
	The assumed test statistic / distribution of the data.  Currently only "Normal" and "CUSUM" supported.
}
  \item{class}{
	Logical. If TRUE then an object of class \code{cpt} is returned.
}
  \item{param.estimates}{
	Logical. If TRUE and class=TRUE then parameter estimates are returned. If FALSE or class=FALSE no parameter estimates are returned.
}
\item{shape}{
Only used when cost function is the gamma likelihood.
}
\item{tol}{
The tolerance at which model decisions are made. Only used within type="reg".
}
  \item{know.mean}{
	Only required for test.stat="Normal".  Logical, if TRUE then the mean is assumed known and mu is taken as its value.  If FALSE, and mu=NA (default value) then the mean is estimated via maximum likelihood.  If FALSE and the value of mu is supplied, mu is not estimated but is counted as an estimated parameter for decisions. Only used within type="var".
}
  \item{mu}{
	Only required for test.stat="Normal".  Numerical value of the true mean of the data.  Either single value or vector of length nrow(data).  If data is a matrix and mu is a single value, the same mean is used for each row. Only used within type="var".
}
  \item{minseglen}{
  Positive integer giving the minimum segment length (no. of observations between changes), default is the minimum allowed by theory.
  }
}
\details{
	This function is used to plot the changes in mean and variance.
}
\value{
 Plots data.
}
\references{
Change in Normal mean: Hinkley, D. V. (1970) Inference About the Change-Point in a Sequence of Random Variables, \emph{Biometrika} \bold{57}, 1--17

PELT Algorithm: Killick R, Fearnhead P, Eckley IA (2012) Optimal detection of changepoints with a linear computational cost, \emph{JASA} \bold{107(500)}, 1590--1598

MBIC: Zhang, N. R. and Siegmund, D. O. (2007) A Modified Bayes Information Criterion with Applications to the Analysis of Comparative Genomic Hybridization Data. \emph{Biometrics} \bold{63}, 22-32.
}
\author{
Andrew Connell, Rebecca Killick
}


\seealso{
\code{\link{ocpt.var.initialise}},\code{\link{ocpt.mean.initialise}},\code{\linkS4class{ocpt}}
}
\examples{
set.seed(1)
x=c(rnorm(50,0,1),rnorm(50,15,1),rnorm(50,30,1),rnorm(50,50,1))
ocpt.plot(x, Q=5)
}

\keyword{methods}
\keyword{univar}
\keyword{models}
\keyword{ts}
