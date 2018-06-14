\name{online.data_input}
\alias{online.data_input}
\title{
Inputs required fields in to single.norm and multiple.norm functions - Only intended for developer use.
}
\description{
This function is called by \code{ocpt.mean}, \code{ocpt.var} and \code{ocpt.meanvar}.  This is not intended for use by regular users of the package.  It is exported for developers to call directly for speed and convenience.

WARNING: No checks on arguments are performed!
}
\usage{
online.data_input <- function(data, method="PELT", pen.value, costfunc, minseglen, Q, var=0, shape=1)
}
\arguments{
  \item{data}{
  Data used in changepoint analysis, see \code{\link{ocpt.mean}} for further details.
}
  \item{method}{
	Method used as a text string, see \code{\link{ocpt.mean}} for further details.
}
  \item{pen.value}{
	Numerical penalty value used in the analysis (positive).
}
 \item{costfunc}{
 The cost function for a given data set and method.
 }
  \item{minseglen}{
  Minimum segment length used in the analysis (positive integer).
}
 \item{var}{
 Used in \code{SEGNEIGH.online} to set the variance.
}
  \item{Q}{
	The value of \code{Q} used in the \code{BinSeg} or \code{SegNeigh} methods.
}
  \item{shape}{
	Value of the assumed known shape parameter required when test.stat="Gamma".
}
}
\details{
This function is called by \code{ocpt.mean}, \code{ocpt.var} and \code{ocpt.meanvar}.  This is not intended for use by regular users of the package.  It is exported for developers to call directly for speed and convenience.
\value{
  An object of class \code{ocpt} or \code{ocpt.range} as appropriate filled with the given attributes.
}
\author{
Rebecca Killick
}
\seealso{
\code{\link{ocpt.var}},\code{\link{ocpt.mean}},\code{\link{plot-methods}},\code{\linkS4class{ocpt}}
}
\examples{
#This function should only be used by developers, see its use in ocpt.mean, ocpt.var and ocpt.meanvar.
}

\keyword{methods}
\keyword{univar}
\keyword{models}
\keyword{ts}
