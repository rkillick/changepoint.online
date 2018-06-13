\name{online.class_input}
\alias{online.class_input}
\title{
Input all required arguments into ocpt classes - Only intended for developer use.
}
\description{
This function helps to input all the necessary information into the correct format for \code{ocpt} and \code{ocpt.range} classes.

This function is called by \code{ocpt.mean}, \code{ocpt.var} and \code{ocpt.meanvar} when \code{class=TRUE}.  This is not intended for use by regular users of the package.  It is exported for developers to call directly for speed and convenience.

WARNING: No checks on arguments are performed!
}
\usage{
online.class_input(data, ocpttype, method, test.stat, penalty, pen.value, minseglen, 
param.estimates, out=list(), Q=NA, shape=NA)
}
\arguments{
  \item{data}{
  Data used in changepoint analysis, see \code{\link{ocpt.mean}} for further details.
}
  \item{ocpttype}{
  Type of changepoint analysis performed as a text string, e.g. "Mean", "Mean and Variance".
}
  \item{method}{
	Method used as a text string, see \code{\link{ocpt.mean}} for further details.
}
  \item{test.stat}{
	The assumed test statistic / distribution of the data as a text string.  , see \code{\link{ocpt.mean}}, \code{\link{ocpt.meanvar}} or \code{\link{ocpt.var}} for further details.
}
  \item{penalty}{
	Penalty used as a text string, see \code{\link{ocpt.mean}} for further details.
}
  \item{pen.value}{
	Numerical penalty value used in the analysis (positive).
}
  \item{minseglen}{
  Minimum segment length used in the analysis (positive integer).
}
  \item{param.estimates}{
	Logical.  If TRUE then parameter estimates are calculated. If FALSE no parameter estimates are calculated and the slot is blank in the returned object.
}
  \item{out}{
  List of output from \code{\link{PELT.online}} or other \code{method} used.  Function assumes that \code{method} and format of \code{out} match.
}
  \item{Q}{
	The value of \code{Q} used in the \code{BinSeg} or \code{SegNeigh} methods.
}
  \item{shape}{
	Value of the assumed known shape parameter required when test.stat="Gamma".
}
}
\details{
	This function takes all the input required for the \code{ocpt} or \code{ocpt.range} classes and enters it into the object.
	
		This function is exported for developer use only.  It does not perform any checks on inputs and is simply a convenience function for converting the output of the worker functions into a nice format for the \code{ocpt} and \code{ocpt.range} classes.
}
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
