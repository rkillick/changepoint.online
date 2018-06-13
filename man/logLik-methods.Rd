\name{logLik-methods}
\docType{methods}
\alias{logLik-methods}
\alias{logLik,ocpt-method}
\alias{logLik,ocpt.reg-method}
\alias{logLik,ocpt.range-method}
\title{ ~~ Methods for Function logLik  ~~}
\description{
 ~~ Methods for function \code{logLik}  ~~
}
\section{Methods}{
\describe{

\item{\code{signature(object = "ocpt")}}{
	Returns the -2*log-likelihood of the data with the fitted changepoints, two values are returned, the raw twice negative log-likelihood and twice negative log-likelihood + penalty.  Only valid for ocpttype="mean","variance" or "mean and variance".
}
}}
\examples{
set.seed(1)
x=c(rnorm(50,0,1),rnorm(50,0,10),rnorm(50,0,5),rnorm(50,0,1))
out=ocpt.var(x,penalty="Manual",pen.value="2*log(n)",method="BinSeg",Q=5)
logLik(out) # returns the raw scaled negative likelihood (925.8085) and the scaled negative
#likelihood + penalty (957.5984)
}

\keyword{methods}
\keyword{ocpt}
\keyword{internal} 