\name{online.ecp.class_input}
\alias{online.ecp.class_input}
\title{
Input all required arguments into online cpt classes - Only intended for developer use.
}
\description{
This function helps to input all the necessary information into the correct format for the \code{ecp.ocpt} class.

This function is called by \code{ocpt.mean.initialise}, \code{ocpt.var.initialise}, \code{ocpt.meanvar.initialise}, \code{ocpt.np.initialise} and all the corresponding update functions. This is not intended for use by regular users of the package.  It is exported for developers to call directly for speed and convenience.

WARNING: No checks on arguments are performed!
}
\usage{
online.ecp.class_input(number, estimates, GofM, delta, alpha,
verbose, csum, dll, dlr, drr, left, right, datalength, functime, width, cpLoc)
}
\arguments{
\item{number}{The estimated number of change points.}
\item{estimates}{The location of the change points estimated by the procedure.}
\item{GofM}{Goodness of fit Model.}
\item{delta}{The window size used to calculate the calculate the complete portion of our approximate test statistic.}
\item{alpha}{The moment index used for determining the distance between and within segments.}
\item{verbose}{A flag indicating if status updates should be printed.}
\item{csum}{The cumulative distance sum of all datapoints.}
\item{dll}{Sum of within sample distances for left segments.}
\item{dlr}{Between sample distance sum using dll and drr.}
\item{drr}{Sum of within sample distances for right segments.}
\item{left}{A vector which gives the sum of distances before point i at Left(i,0). Left(i,1) is to resolve out of bound errors.}
\item{right}{A vector which gives the sum of distances after point i at Right(i,0). Right(i,1) is to resolve out of bound errors.}
\item{datalength}{This is the total length of the observations.}
\item{functime}{The total amount to time take to estimate the change point locations.}
\item{width}{This is the number of changepoints located in the dataset.}
\item{cpLoc}{The list of locations of change points estimated by the procedure for different numbers of change points up to K (the maximum number of changepoints the user wishes to search for).}
}
\details{
	This function takes all the input required for the \code{ecp.ocpt} class and enters it into the object.
	
		This function is exported for developer use only.  It does not perform any checks on inputs and is simply a convenience function for converting the output of the worker functions into a nice format for the class.
}
\value{
  An object of class \code{ecp.ocpt} filled with the given attributes.
}
\author{
Andrew Connell,
Rebecca Killick
}
\seealso{
\code{\link{ocpt.var.initialise}},\code{\link{ocpt.mean.initialise}},\code{\linkS4class{ecp.ocpt}}
}
\examples{
#This function should only be used by developers, 
#see its use in ocpt.mean, ocpt.var and ocpt.meanvar.
}

\keyword{methods}
\keyword{univar}
\keyword{models}
\keyword{ts}
