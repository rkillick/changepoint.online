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
	Returns the -2*log-likelihood of the data with the fitted changepoints, two values are returned, the raw twice negative log-likelihood and twice negative log-likelihood + penalty.  Only valid for cpttype="mean","variance" or "mean and variance".
}
}}
\examples{
set.seed(1)
x=c(rnorm(50,0,1),rnorm(50,0,10),rnorm(50,0,5),rnorm(50,0,1))
  out=ocpt.var.initialise(x, penalty = "Manual", pen.value = 2)
logLik(out)
}

\keyword{methods}
\keyword{ocpt}
\keyword{internal} 