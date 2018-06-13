\name{ocpt.var}
\alias{ocpt.var}
\title{
Identifying Changes in Variance
}
\description{
Calculates the optimal positioning and (potentially) number of changepoints for data using the user specified method.
}
\usage{
ocpt.var(data,penalty="MBIC",pen.value=0,know.mean=FALSE,mu=NA,method="AMOC",Q=5,
test.stat="Normal",class=TRUE,param.estimates=TRUE,minseglen=2)
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
  \item{know.mean}{
	Only required for test.stat="Normal".  Logical, if TRUE then the mean is assumed known and mu is taken as its value.  If FALSE, and mu=NA (default value) then the mean is estimated via maximum likelihood.  If FALSE and the value of mu is supplied, mu is not estimated but is counted as an estimated parameter for decisions.
}
  \item{mu}{
	Only required for test.stat="Normal".  Numerical value of the true mean of the data.  Either single value or vector of length nrow(data).  If data is a matrix and mu is a single value, the same mean is used for each row.
}
  \item{method}{
	Choice of "AMOC", "PELT", "SegNeigh" or "BinSeg".
}
  \item{Q}{
	The maximum number of changepoints to search for using the "BinSeg" method.  The maximum number of segments (number of changepoints + 1) to search for using the "SegNeigh" method.
}
  \item{test.stat}{
	The assumed test statistic / distribution of the data.  Currently only "Normal" and "CSS" supported.
}
  \item{class}{
	Logical.  If TRUE then an object of class \code{ocpt} is returned.
}
  \item{param.estimates}{
	Logical.  If TRUE and class=TRUE then parameter estimates are returned. If FALSE or class=FALSE no parameter estimates are returned.
}
  \item{minseglen}{
  Positive integer giving the minimum segment length (no. of observations between changes), default is the minimum allowed by theory.
  }
}
\details{
	This function is used to find changes in variance for data using the test statistic specified in the test.stat parameter.  The changes are found using the method supplied which can be single changepoint (AMOC) or multiple changepoints using exact (PELT or SegNeigh) or approximate (BinSeg) methods.  A changepoint is denoted as the last observation of the segment / regime.
  Note that for the test.stat="CSS" option the preset penalties are log(.) to allow comparison with test.stat="Normal".  
}
\value{
	If \code{class=TRUE} then an object of S4 class "ocpt" is returned.  The slot \code{ocpts} contains the changepoints that are returned.  For \code{class=FALSE} the structure is as follows.

	If data is a vector (single dataset) then a vector/list is returned depending on the value of method.  If data is a matrix (multiple datasets) then a list is returned where each element in the list is either a vector or list depending on the value of method.

	If method is AMOC then a vector (one dataset) or matrix (multiple datasets) is returned, the columns are:
	\item{ocpt}{The most probable location of a changepoint if a change was identified or NA if no changepoint.}
  \item{p value}{The p-value of the identified changepoint.}
	If method is PELT then a vector is returned containing the changepoint locations for the penalty supplied.  This always ends with n.
  If the penalty is CROPS then a list is returned with elements:
  \item{ocpt.out}{A data frame containing the value of the penalty value where the number of segmentations changes, the number of segmentations and the value of the cost at that penalty value.}
  \item{segmentations}{The optimal segmentations for the different penalty values starting with the lowest penalty value}
	If method is SegNeigh then a list is returned with elements:
	\item{cps}{Matrix containing the changepoint positions for 1,...,Q changepoints.}
	\item{op.ocpts}{The optimal changepoint locations for the penalty supplied.}
  \item{pen}{Penalty used to find the optimal number of changepoints.}
	\item{like}{Value of the -2*log(likelihood ratio) + penalty for the optimal number of changepoints selected.}
	If method is BinSeg then a list is returned with elements:
	\item{cps}{2xQ Matrix containing the changepoint positions on the first row and the test statistic on the second row.}
	\item{op.ocpts}{The optimal changepoint locations for the penalty supplied.}
	\item{pen}{Penalty used to find the optimal number of changepoints.}
}
\references{
Normal: Chen, J. and Gupta, A. K. (2000) \emph{Parametric statistical change point analysis}, Birkhauser

CSS: C. Inclan, G. C. Tiao (1994) Use of Cumulative Sums of Squares for Retrospective Detection of Changes of Variance, \emph{Journal of the American Statistical Association} \bold{89(427)}, 913--923

PELT Algorithm: Killick R, Fearnhead P, Eckley IA (2012) Optimal detection of changepoints with a linear computational cost, \emph{JASA} \bold{107(500)}, 1590--1598

CROPS: Haynes K, Eckley IA, Fearnhead P (2014) Efficient penalty search for multiple changepoint problems (in submission), arXiv:1412.3617

Binary Segmentation: Scott, A. J. and Knott, M. (1974) A Cluster Analysis Method for Grouping Means in the Analysis of Variance, \emph{Biometrics} \bold{30(3)}, 507--512

Segment Neighbourhoods: Auger, I. E. And Lawrence, C. E. (1989) Algorithms for the Optimal Identification of Segment Neighborhoods, \emph{Bulletin of Mathematical Biology} \bold{51(1)}, 39--54

MBIC: Zhang, N. R. and Siegmund, D. O. (2007) A Modified Bayes Information Criterion with Applications to the Analysis of Comparative Genomic Hybridization Data. \emph{Biometrics} \bold{63}, 22-32.
}
\author{
Rebecca Killick
}


\seealso{
\code{\link{ocpt.mean}},\code{\link{ocpt.meanvar}},\code{\link{plot-methods}},\code{\linkS4class{ocpt}}
}
\examples{
# Example multiple datasets where the first row has multiple changes in variance and the second row
#has no change in variance
set.seed(10)
x=c(rnorm(50,0,1),rnorm(50,0,10),rnorm(50,0,5),rnorm(50,0,1))
y=rnorm(200,0,1)
z=rbind(x,y)
ocpt.var(z,penalty="MBIC",pen.value=0.01,method="PELT",Q=5,class=FALSE) # returns list that
#has two elements, the first has 3 changes in variance at 50,100,149 and the second has no changes in variance
ans=ocpt.var(z,penalty="MBIC",pen.value=0.01,method="PELT") 
ocpts(ans[[1]]) # same results as for the above method.
ocpts(ans[[2]]) # same results as for the above method.
}

\keyword{methods}
\keyword{univar}
\keyword{models}
\keyword{ts}
