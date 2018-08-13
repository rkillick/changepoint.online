\name{online.class_input}
\alias{online.class_input}
\title{
Input all required arguments into online cpt classes - Only intended for developer use.
}
\description{
This function helps to input all the necessary information into the correct format for \code{ocpt} and \code{ocpt.range} classes.

This function is called by \code{ocpt.mean.initialise}, \code{ocpt.var.initialise} and \code{ocpt.meanvar.initialise} and all the corresponding update functions.  This is not intended for use by regular users of the package.  It is exported for developers to call directly for speed and convenience.

WARNING: No checks on arguments are performed!
}
\usage{
online.class_input(sumstat, cpttype, method, test.stat, penalty, pen.value, minseglen, param.estimates, out = list(), Q = NA, shape = NA, lastchangelike = c(0), lastchangecpts =c(0), checklist = c(0), nchecklist=0, ndone = 0, nupdate = length(data), cost_func)
}
\arguments{
  \item{sumstat}{
  Summary statistic used in changepoint analysis, see \code{\link{ocpt.mean.initalise}} for further details.
}
  \item{cpttype}{
  Type of changepoint analysis performed as a text string, e.g. "Mean", "Mean and Variance".
}
  \item{method}{
	Method used as a text string, see \code{\link{ocpt.mean.initialise}} for further details.
}
  \item{test.stat}{
	The assumed test statistic / distribution of the data as a text string.  , see \code{\link{ocpt.mean}}, \code{\link{ocpt.meanvar}} or \code{\link{ocpt.var}} for further details.
}
  \item{penalty}{
	Penalty used as a text string, see \code{\link{ocpt.mean.initialise}} for further details.
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
\item{lastchangelike}{
	Vector of lenght n containing the likelihood of the optimal segmentation up to each timepoint.
}
\item{lastchangecpts}{
	Vector of length n containing the last changepoint prior to each timepoint.
}
\item{checklist}{
	Vector of locations of the potential last changepoint for next iteration (to be updated), max length=(ndone+nupdate).
}
\item{nchecklist}{
Stores the current number of changepoints detected.
}
\item{ndone}{
Length of the time series when analysis begins.
}
\item{nupdate}{
	Length of the time series to be analysed in this update.
}
\item{cost_func}{
The cost function that has been used.
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
Andrew Connell, Rebecca Killick
}
\seealso{
\code{\link{ocpt.var.initialise}},\code{\link{ocpt.mean.initialise}},\code{\linkS4class{ocpt}}
}
\examples{
#This function should only be used by developers, see its use in ocpt.mean, ocpt.var and ocpt.meanvar.
}

\keyword{methods}
\keyword{univar}
\keyword{models}
\keyword{ts}
