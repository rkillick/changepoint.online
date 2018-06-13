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
    \item{\code{ocpts.full}:}{Object of class \code{"matrix"}, each row of the matrix is a different segmentation of the data (different set of online changepoints).}
    \item{\code{pen.value.full}:}{Object of class \code{"vector"}, each element is the penalty used to create the set of changepoints in the corresponding row of \code{ocpts.full}.}
    The remaining slots are inherited from the \code{ocpt} class.
    \item{\code{data.set}:}{Object of class \code{"ts"}, a coerced time series of the original data. Inherited from ocpt class. }
    \item{\code{ocpttype}:}{Object of class \code{"character"}, the type of online changepoint that was identified. Inherited from ocpt class. }
    \item{\code{method}:}{Object of class \code{"character"}, the method that was used to search for online changepoints. Inherited from ocpt class. }
    \item{\code{test.stat}:}{Object of class \code{"character"}, the test statistic for the analysis of the data. Inherited from ocpt class. }
    \item{\code{pen.type}:}{Object of class \code{"character"}, the penalty type specified in the analysis. Inherited from ocpt class.}
    \item{\code{pen.value}:}{Object of class \code{"numeric"}, the value of the penalty used in the analysis. Inherited from ocpt class.}
    \item{\code{minseglen}:}{Object of class \code{"numeric"}, the minimum segment length (no. of observations between online changepoints) used in the analysis. Inherited from ocpt class.}
    \item{\code{ocpts}:}{Object of class \code{"numeric"}, vector of optimal online changepoints identified. Inherited from ocpt class. }
    \item{\code{nocpts.max}:}{Object of class \code{"numeric"}, maximum number of online changepoint that can be identified. Inherited from ocpt class. }
    \item{\code{param.est}:}{Object of class \code{"list"}, list where each element is a vector of parameter estimates, if requested. Inherited from ocpt class. }
    \item{\code{date}:}{Object of class \code{"character"}, date and time the online changepoint analysis was run. Inherited from ocpt class. }
    \item{\code{version}:}{Object of class \code{"character"}, version number of the package used when the analysis was run. Inherited from ocpt class.}
  }
}
\section{Methods}{
  \describe{
    \item{ocpts.full}{\code{signature(object = "ocpt.range")}: retrieves ocpts.full slot }
    \item{pen.value.full}{\code{signature(object = "ocpt.range")}: retrieves pen.value.full slot }
    \item{ocpts.full<-}{\code{signature(object = "ocpt.range")}: replaces ocpts.full slot }
    \item{param}{\code{signature(object="ocpt.range",nocpts=NA)}: creates parameter estimates for the segmentation with \code{nocpts} number of online changepoints.  If nocpts=NA then the optimal set of online changepoints according to the set penalty is used.}
    \item{pen.value.full<-}{\code{signature(object = "ocpt.range")}: replaces pen.value.full slot }
    \item{plot}{\code{signature(object="ocpt.range",nocpts=NA,diagnostic=FALSE)}: by default plots the optimal segmentation as for \code{class="ocpt"}.  If nocpts is specified then plots the segmentation for \code{nocpts} number of online changepoints.  If \code{diagnostic=TRUE} then produces a diagnostic plot to aide selection of the number of changes.}
    \item{print}{\code{signature(object = "ocpt.range")}: prints details of the ocpt.range object including summary}
    \item{summary}{\code{signature(object = "ocpt.range")}: prints a summary of the ocpt.range object }
  }
}

\author{
Rebecca Killick
}

\seealso{
\code{\link{ocpts.full-methods}},\code{\linkS4class{ocpt}},\code{\link{ocpt.mean}},\code{\link{ocpt.var}},\code{\link{ocpt.meanvar}}
}
\examples{
showClass("ocpt.range") # shows the structure of the ocpt.range class

x=new("ocpt.range") # creates a new object with the ocpt.range class defaults
ocpts(x) # retrieves the ocpts slot from x

# Example of multiple changes in variance at 50,100,150 in simulated normal data
set.seed(1)
x=c(rnorm(50,0,1),rnorm(50,0,10),rnorm(50,0,5),rnorm(50,0,1))
out=ocpt.var(x,pen.value=c(log(length(x)),10*log(length(x))),method="PELT")
print(out) # prints details of the analysis including a summary
summary(out)
plot(out,diagnostic=TRUE) # a diagnostic plot to identify number of changepoints
# looks like the segmentation with 3 changepoints, 50,99,150 is the most appropriate
plot(out,nocpts=3) # plots the segmentation for 3 changes
}
\keyword{classes}
\keyword{ocpt}
\keyword{internal} 