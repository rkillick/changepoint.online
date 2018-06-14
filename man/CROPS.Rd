\name{CROPS}
\alias{CROPS}
\title{
CROPS() - Only intended for developer use.
}
\description{
Implements the CROPS method for identifying changepoints in a given set of summary statistics for a specified cost function and penalty.

WARNING: No checks on arguments are performed!
}
\usage{
CROPS(data, penalty="CROPS", pen.value, costfunc="norm.mean", method="PELT", test.stat="Normal", class=TRUE, param.est=TRUE, minseglen, shape, func)
}
\arguments{
  \item{data}{
	A vector, ts object or matrix containing the data within which you wish to find a changepoint.  If data is a matrix, each row is considered a separate dataset.
}
  \item{Penalty}{
  hoice of "None", "SIC", "BIC", "MBIC", AIC", "Hannan-Quinn", "Asymptotic", "Manual" and "CROPS" penalties.  If Manual is specified, the manual penalty is contained in the pen.value parameter. If Asymptotic is specified, the theoretical type I error is contained in the pen.value parameter.  If CROPS is specified, the penalty range is contained in the pen.value parameter; note this is a vector of length 2 which contains the minimum and maximum penalty value.  Note CROPS can only be used if the method is "PELT". The predefined penalties listed DO count the changepoint as a parameter, postfix a 0 e.g."SIC0" to NOT count the changepoint as a parameter. This is used in \code{online.class_input}.
}
\item{pen.value}{
  The theoretical type I error e.g.0.05 when using the Asymptotic penalty.  A vector of length 2 (min,max) if using the CROPS penalty.  The value of the penalty when using the Manual penalty option - this can be a numeric value or text giving the formula to use.  Available variables are, n=length of original data, null=null likelihood, alt=alternative likelihood, tau=proposed changepoint, diffparam=difference in number of alternatve and null parameters.  
}
  \item{costfunc}{
  The costfunc will be called by PELT.online and is used to calculate the range of penalties.
}
\item{method}{
	Choice of "AMOC", "PELT", "SegNeigh" or "BinSeg".
	}
\item{test.stat}{
	The assumed test statistic / distribution of the data.  Currently only "Normal" and "CUSUM" supported.
}
 \item{class}{
	Logical.  If TRUE then an object of class \code{cpt} is returned.
}
 \item{param.est}{
	Logical.  If TRUE and class=TRUE then parameter estimates are returned. If FALSE or class=FALSE no parameter estimates are returned.
}
  \item{minseglen}{
  Positive integer giving the minimum segment length (no. of observations between changes), default is 1.  No checks are performed on the input value so it could be larger than feasible to have changes in the data.
  }
    \item{shape}{
	Only required for costfunc="Gamma",default is 1.  Must be a positive value, this isn't checked.
}
\item{func}{
Choice of "var", "meanvar" or "mean". This dictates the ocptype used in \code{online.class_input}
}
}
\details{
This function uses the CROP method to calculate the changepoints of a data set.
}
\value{
	A list is returned with elements:
	\item{Changepoint Type}{Either shows changes in variance, mean, both or neither.}
	\item{Method of Analysis}{Outputs the method chosen, if no method chose then output will be PELT}
	\item{Test Statistic}{The test.stat that has been inputted.}
	\item{Type of Penalty}{Penalty chosen and specifies values using \code{pen.value}}
	\item{Minimum Segment Length}{The chosen value for \code{minseglen}}
	\item{Maximum no. of cpts}{The maximum number of changepoints that could exist in the data}
	\item{Changepoint Locations}{The location of each changepoint}
	\item{Range of segmentations}{Where the data has been divided}
	\item{For penalty values:}{Which penalty value has been used}
}
\references{
PELT Algorithm: Killick R, Fearnhead P, Eckley IA (2012) Optimal detection of changepoints with a linear computational cost, \emph{JASA} \bold{107(500)}, 1590--1598

CROPS: Haynes K, Eckley IA, Fearnhead P (2014) Efficient penalty search for multiple changepoint problems (in submission), arXiv:1412.3617
}
\author{
Rebecca Killick
}


\seealso{
\code{\link{Pelt.online}}
}
\examples{
#This function should only be used by developers, see its use in ocpt.mean, ocpt.var and ocpt.meanvar.
}

\keyword{methods}
\keyword{univar}
\keyword{models}
\keyword{ts}
