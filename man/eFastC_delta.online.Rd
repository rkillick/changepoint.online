\name{eFastC_delta.online}
\alias{eFastC_delta.online}
\title{
eFastC_delta.online - wrapper function.
}
\description{
This is the wrapper function to hand \code{e.cp3o_delta.online.initialise} and \code{e.cp3o_delta.online.update} in to C++. This function is exported for developer use only. 
}
\usage{
eFastC_delta.online(Z_, K_, delta_, alpha_, verbose_, oldlength_,
newlength_, cSum_, DLL_, DRR_, DLR_, Left_, Right_)
}
\arguments{
  \item{Z_}{
	A matrix containing the data.
}
  \item{K_}{
 The maximum number of changepoints the user wishes to search for.
}
  \item{delta_}{
  The window size used to calculate the calculate the complete portion of our approximate test statistic. This also corresponds to one less than the minimum segment size. It is the same as minseglen for PELT.
}
  \item{alpha_}{
	The moment index used for determining the distance between and within segments.
}
  \item{verbose_}{
  A flag indicating if status updates should be printed.
  }
  \item{oldlength_}{
  The number of data points previously. This is only used for update functions.
  }
  \item{newlength_}{
  The number of data points being observed in this calculation.
  }
  \item{cSum_}{
   The cumulative distance sum of all datapoints and the necessary empty cells to be filled by new data.
  }
  \item{DLL_}{
  Sum of within sample distances for left segments.
  }
  \item{DRR_}{
  Sum of within sample distances for right segments.
  }
  \item{DLR_}{
  Between sample distance sum using dll and drr.
  }
  \item{Left_}{
  A vector which gives the sum of distances before point i at Left(i,0).
  }
  \item{Right_}{
  A vector which gives the sum of distances after point i at Right(i,0).
  }
}
\details{
	This function is used as a wrapper function to implement the ECP algorithm in C++.  It simply hands across the necessary worker vectors, ensures all inputs are the correct type, and passes everything to the C++ function.
}
\value{
	A list is returned:
	\item{number}{The number of estimated change points.}
	\item{estimates}{The location of the change points estimated by the procedure.}
	\item{GofM}{Goodness of Fit Model's score.}
	\item{delta}{The window size.}
	\item{alpha}{The moment index used.}
	\item{verbose}{A flag indicating if status updates should be printed.}
	\item{csum}{ The cumulative distance sum of all datapoints and the necessary empty cells to be filled by new data.}
	\item{dll}{Sum of within sample distances for left segments.}
	\item{dlr}{Between sample distance sum using dll and drr.}
	\item{drr}{Sum of within sample distances for right segments.}
	\item{left}{A vector which gives the sum of distances before point i at Left(i,0).}
	\item{right}{A vector which gives the sum of distances after point i at Right(i,0).}
	
}
\author{
Andrew Connell,
Rebecca Killick,
David Matteson
}
\keyword{methods}
\keyword{univar}
\keyword{models}
\keyword{ts}
