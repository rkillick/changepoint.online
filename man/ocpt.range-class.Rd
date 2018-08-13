\name{ocpt.range-class}
\Rdversion{1.1}
\docType{class}
\alias{ocpt.range-class}

\title{Class "ocpt.range"}
\description{
A class for online changepoint objects that return more than 1 segmentation. Inherits from ocpt class.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("ocpt.range", ...)}.
  \describe{
    \item{\code{new("ocpt.range", ...)}:}{creates a new object with class ocpt.range }
  }
}
\section{Slots}{
  \describe{
    \item{\code{cpts.full}:}{Object of class \code{"matrix"}, each row of the matrix is a different segmentation of the data (different set of online changepoints).}
    \item{\code{pen.value.full}:}{Object of class \code{"vector"}, each element is the penalty used to create the set of changepoints in the corresponding row of \code{ocpts.full}.}
    The remaining slots are inherited from the \code{ocpt} class.
    \item{\code{sumstat}:}{Object of class \code{"array"}, a summary statistic matrix of the original data. Inherited from ocpt class. }
    \item{\code{cpttype}:}{Object of class \code{"character"}, the type of online changepoint that was identified. Inherited from ocpt class. }
    \item{\code{method}:}{Object of class \code{"character"}, the method that was used to search for online changepoints. Inherited from ocpt class. }
    \item{\code{test.stat}:}{Object of class \code{"character"}, the test statistic for the analysis of the data. Inherited from ocpt class. }
    \item{\code{pen.type}:}{Object of class \code{"character"}, the penalty type specified in the analysis. Inherited from ocpt class.}
    \item{\code{pen.value}:}{Object of class \code{"numeric"}, the value of the penalty used in the analysis. Inherited from ocpt class.}
    \item{\code{minseglen}:}{Object of class \code{"numeric"}, the minimum segment length (no. of observations between online changepoints) used in the analysis. Inherited from ocpt class.}
    \item{\code{cpts}:}{Object of class \code{"numeric"}, vector of optimal online changepoints identified. Inherited from ocpt class. }
    \item{\code{ncpts.max}:}{Object of class \code{"numeric"}, maximum number of online changepoint that can be identified. Inherited from ocpt class. }
    \item{\code{param.est}:}{Object of class \code{"list"}, list where each element is a vector of parameter estimates, if requested. Inherited from ocpt class. }
    \item{\code{date}:}{Object of class \code{"character"}, date and time the online changepoint analysis was run. Inherited from ocpt class. }
    \item{\code{version}:}{Object of class \code{"character"}, version number of the package used when the analysis was run. Inherited from ocpt class.}
    \item{\code{lastchangelike}:}{Object of class \code{"numeric"}, vector of lenght n containing the likelihood of the optimal segmentation up to each timepoint.}
    \item{\code{lastchangecpts}:}{Object of class \code{"numeric"}, vector of length n containing the last changepoint prior to each timepoint.}
    \item{\code{nchecklist}:}{Object of class \code{"numeric"}, stores the current number of changepoints detected.}
    \item{\code{checklist}:}{Object of class \code{"numeric"}, vector of locations of the potential last changepoint for next iteration (to be updated), max length=(ndone+nupdate).}
    \item{\code{ndone}:}{Object of class \code{"numeric"}, length of the time series when analysis begins.}
    \item{\code{nupdate}:}{Object of class \code{"numeric"}, length of the time series to be analysed in this update.}
    \item{\code{cost_func}:}{Object of class \code{"character"}, the cost function used in PELT.online calculations.}
    \item{\code{shape}:}{Object of class \code{"numeric"}, only used when cost_func is the gamma likelihood. Otherwise 1.}
  }
}
\section{Methods}{
  \describe{
    \item{cpts.full}{\code{signature(object = "ocpt.range")}: retrieves ocpts.full slot }
    \item{pen.value.full}{\code{signature(object = "ocpt.range")}: retrieves pen.value.full slot }
    \item{cpts.full<-}{\code{signature(object = "ocpt.range")}: replaces ocpts.full slot }
    \item{param}{\code{signature(object="ocpt.range",nocpts=NA)}: creates parameter estimates for the segmentation with \code{nocpts} number of online changepoints.  If nocpts=NA then the optimal set of online changepoints according to the set penalty is used.}
    \item{pen.value.full<-}{\code{signature(object = "ocpt.range")}: replaces pen.value.full slot }
    \item{plot}{\code{signature(object="ocpt.range",nocpts=NA,diagnostic=FALSE)}: by default plots the optimal segmentation as for \code{class="ocpt"}.  If nocpts is specified then plots the segmentation for \code{nocpts} number of online changepoints.  If \code{diagnostic=TRUE} then produces a diagnostic plot to aide selection of the number of changes.}
    \item{print}{\code{signature(object = "ocpt.range")}: prints details of the ocpt.range object including summary}
    \item{summary}{\code{signature(object = "ocpt.range")}: prints a summary of the ocpt.range object }
  }
}

\author{
Andrew Connell, Rebecca Killick
}

\seealso{
\code{\link{cpts.full-methods}},\code{\linkS4class{ocpt}}
}
\examples{
showClass("ocpt.range") # shows the structure of the ocpt.range class

# Example of multiple changes in variance at 50,100,150 in simulated normal data
set.seed(1)
x=c(rnorm(50,0,1),rnorm(50,0,10),rnorm(50,0,5),rnorm(50,0,1))
out=ocpt.var.initialise(x,pen.value=c(log(length(x)),10*log(length(x))))
print(out) # prints details of the analysis including a summary
summary(out)
}
\keyword{classes}
\keyword{ocpt}
\keyword{internal} 