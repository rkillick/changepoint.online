\name{param-methods}
\docType{methods}
\alias{param-methods}
\alias{param,ocpt-method}
\alias{param,ocpt.range-method}
\alias{param,ocpt.reg-method}
\title{ ~~ Methods for Function param  ~~}
\description{
 ~~ Methods for function \code{param}  ~~
}
\section{Methods}{
\describe{

\item{\code{signature(object = "ocpt")}}{
	Replaces the \code{param.value} slot in object with the parameter estimates that are appropriate for the changepoint type (\code{ocpttype} slot).  If the Gamma test statistic is used then the shape parameter is required as a variable.
}
\item{\code{signature(object = "ocpt.range")}}{
	Returns a new (blank) ocpt.range object where the \code{param.value} slot contains the parameter estimates for a segmentation with the \code{nocpts} specified number of changepoints.  This is also tailored to the changepoint type (\code{ocpttype} slot) of the original object.  If the Gamma test statistic is used then the shape parameter is required as a variable.  An example of use is given in the \link{ocpt.mean} help file.
}
\item{\code{signature(object = "ocpt.reg")}}{
	Replaces the \code{param.value} slot in object with the parameter estimates that are appropriate for the changepoint test statistic (\code{test.stat} slot).
}
}}
\keyword{methods}
\keyword{ocpt}
\keyword{internal} 