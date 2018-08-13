\name{ocpt-class}
\Rdversion{1.1}
\docType{class}
\alias{ocpt-class}
\alias{ocpt-method,cpts}
\alias{ocpt-method,cpttype}
\alias{ocpt-method,sumstat}
\alias{ocpt-method,test.stat}
\alias{ocpt-method,ncpts.max}
\alias{ocpt-method,method}
\alias{ocpt-method,minseglen}
\alias{ocpt-method,param.est}
\alias{ocpt-method,pen.type}
\alias{ocpt-method,pen.value}
\alias{ocpt-method,cpts<-}
\alias{ocpt-method,cpttype<-}
\alias{ocpt-method,sumstat<-}
\alias{ocpt-method,test.stat<-}
\alias{ocpt-method,ncpts.max<-}
\alias{ocpt-method,method<-}
\alias{ocpt-method,minseglen<-}
\alias{ocpt-method,param.est<-}
\alias{ocpt-method,pen.type<-}
\alias{ocpt-method,pen.value<-}
\alias{ocpt-method,print}
\alias{ocpt-method,summary}
\alias{ocpt-method,plot}
\alias{ocpt-method,param}
\alias{ocpt-method,logLik}

\title{Class "ocpt"}
\description{
A class for online changepoint objects.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("ocpt", ...)}.
  \describe{
    \item{\code{new("ocpt", ...)}:}{creates a new object with class ocpt }
  }
}
\section{Slots}{
  \describe{
    \item{\code{sumstat}:}{Object of class \code{"array"}, a summary statistic for the the original data. }
    \item{\code{cpttype}:}{Object of class \code{"character"}, the type of online changepoint that was identified. }
    \item{\code{method}:}{Object of class \code{"character"}, the method that was used to search for changepoints. }
    \item{\code{test.stat}:}{Object of class \code{"character"}, the test statistic for the analysis of the data. }
    \item{\code{pen.type}:}{Object of class \code{"character"}, the penalty type specified in the analysis.}
    \item{\code{pen.value}:}{Object of class \code{"numeric"}, the value of the penalty used in the analysis.}
    \item{\code{minseglen}:}{Object of class \code{"numeric"}, the minimum segment length (no. of observations between changepoints) used in the analysis.}
    \item{\code{cpts}:}{Object of class \code{"numeric"}, vector of online changepoints identified. }
    \item{\code{ncpts.max}:}{Object of class \code{"numeric"}, maximum number of online changepoint that can be identified. }
    \item{\code{param.est}:}{Object of class \code{"list"}, list where each element is a vector of parameter estimates, if requested. }
    \item{\code{date}:}{Object of class \code{"character"}, date and time the changepoint analysis was run. }
        \item{\code{version}:}{Object of class \code{"character"}, version number of the package used when the analysis was run.}
 \item{\code{lastchangelike}:}{Object of class \code{"numeric"}, vector of lenght n containing the likelihood of the optimal segmentation up to each timepoint.}
    \item{\code{lastchangecpts}:}{Object of class \code{"numeric"}, vector of length n containing the last changepoint prior to each timepoint.}
    \item{\code{numchangecpts}:}{Object of class \code{"numeric"}, stores the current number of changepoints detected.}
    \item{\code{checklist}:}{Object of class \code{"numeric"}, vector of locations of the potential last changepoint for next iteration (to be updated), max length=(ndone+nupdate).}
    \item{\code{ndone}:}{Object of class \code{"numeric"}, length of the time series when analysis begins.}
    \item{\code{nupdate}:}{Object of class \code{"numeric"}, length of the time series to be analysed in this update.}
    \item{\code{cost_func}:}{Object of class \code{"character"}, the cost function used in PELT.online calculations.}
    \item{\code{shape}:}{Object of class \code{"numeric"}, only used when cost_func is the gamma likelihood. Otherwise 1.}
  }
}
\section{Methods}{
  \describe{
    \item{ocpts}{\code{signature(object = "ocpt")}: retrieves ocpts slot }
    \item{cpttype}{\code{signature(object = "ocpt")}: retrieves ocpttype slot }
    \item{sumstat}{\code{signature(object = "ocpt")}: retrieves matrix version of sumstat slot }
    \item{test.stat}{\code{signature(object = "ocpt")}: retrieves test.stat slot }
    \item{ncpts.max}{\code{signature(object = "ocpt")}: retrieves ncpts.max slot }
    \item{method}{\code{signature(object = "ocpt")}: retrieves method slot }
    \item{minseglen}{\code{signature(object = "ocpt")}: retrieves minseglen slot }
    \item{param.est}{\code{signature(object = "ocpt")}: retrieves param.est slot }
    \item{pen.type}{\code{signature(object = "ocpt")}: retrieves pen.type slot }
    \item{pen.value}{\code{signature(object = "ocpt")}: retrieves pen.value slot }
    \item{cpts<-}{\code{signature(object = "ocpt")}: replaces cpts slot }
    \item{cpttype<-}{\code{signature(object = "ocpt")}: replaces cpttype slot }
    \item{sumstat<-}{\code{signature(object = "ocpt")}: replaces sumstat slot }
    \item{test.stat<-}{\code{signature(object = "ocpt")}: replaces test.stat slot }
    \item{ncpts.max<-}{\code{signature(object = "ocpt")}: replaces ncpts.max slot }
    \item{method<-}{\code{signature(object = "ocpt")}: replaces method slot }
    \item{minseglen<-}{\code{signature(object = "ocpt")}: replaces minseglen slot }
    \item{param.est<-}{\code{signature(object = "ocpt")}: replaces param.est slot }
    \item{pen.type<-}{\code{signature(object = "ocpt")}: replaces pen.type slot }
    \item{pen.value<-}{\code{signature(object = "ocpt")}: replaces pen.value slot }
    \item{print}{\code{signature(object = "ocpt")}: prints details of the cpt object including summary}
    \item{summary}{\code{signature(object = "ocpt")}: prints a summary of the cpt object }
    \item{plot}{\code{signature(object = "ocpt")}: plots the ocpt object with changepoints highlighted}
    \item{param}{\code{signature(object = "ocpt")}: calculates the parameter estimates for the ocpt object}
    \item{logLik}{\code{signature(object = "ocpt")}: returns the overall log-likelihood of the ocpt object}
  }
}

\author{
Andrew Connell,
Rebecca Killick
}

\seealso{
\code{\link{cpts-methods}},\code{\linkS4class{cpt.reg}},\code{\link{ocpt.mean.initialise}},\code{\link{ocpt.var.initialise}},\code{\link{ocpt.meanvar.initialise}}
}
\examples{
showClass("ocpt") # shows the structure of the ocpt class

x=new("ocpt") # creates a new object with the ocpt class defaults
cpts(x) # retrieves the ocpts slot from x

# Example of a change in variance at 100 in simulated normal data
set.seed(1)
x=c(rnorm(100,0,1),rnorm(100,0,10))
ans=ocpt.var.initialise(x) 
print(ans) # prints details of the analysis including a summary
summary(ans)
plot(ans,data=x) # plots the data with change (vertical line) at 100
}
\keyword{classes}
\keyword{ocpt}
\keyword{internal} 