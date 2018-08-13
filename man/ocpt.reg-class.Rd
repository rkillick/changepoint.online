\name{ocpt.reg-class}
\Rdversion{1.1}
\docType{class}
\alias{ocpt.reg-class}
\alias{ocpt.reg-method,cpts}
\alias{ocpt.reg-method,cpttype}
\alias{ocpt.reg-method,sumstat}
\alias{ocpt.reg-method,test.stat}
\alias{ocpt.reg-method,method}
\alias{ocpt.reg-method,ncpts.max}
\alias{ocpt.reg-method,param.est}
\alias{ocpt.reg-method,pen.type}
\alias{ocpt.reg-method,pen.value}
\alias{ocpt.reg-method,cpts<-}
\alias{ocpt.reg-method,cpttype<-}
\alias{ocpt.reg-method,sumstat<-}
\alias{ocpt.reg-method,test.stat<-}
\alias{ocpt.reg-method,method<-}
\alias{ocpt.reg-method,ncpts.max<-}
\alias{ocpt.reg-method,param.est<-}
\alias{ocpt.reg-method,pen.type<-}
\alias{ocpt.reg-method,pen.value<-}
\alias{ocpt.reg-method,print}
\alias{ocpt.reg-method,summary}
\alias{ocpt.reg-method,param}

\title{Class "ocpt.reg"}
\description{
A class for online changepoint objects, specifically change in regression.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("ocpt", ...)}.
  \describe{
    \item{\code{new("ocpt", ...)}:}{creates a new object with class ocpt }
  }
}
\section{Slots}{
  \describe{
    \item{\code{sumstat}:}{Object of class \code{"numeric"}, the summary statistic matrix for the data }
    \item{\code{cpttype}:}{Object of class \code{"character"}, the type of online changepoint that was identified }
    \item{\code{method}:}{Object of class \code{"character"}, the method that was used to search for online changepoints, default change in regression }
    \item{\code{test.stat}:}{Object of class \code{"character"}, the test statistic used to analyse the data }
    \item{\code{pen.type}:}{Object of class \code{"character"}, the penalty type specified in the analysis}
    \item{\code{pen.value}:}{Object of class \code{"numeric"}, the value of the penalty used in the analysis}
    \item{\code{minseglen}:}{Object of class \code{"numeric"}, the minimum segment length (no. of observations between online changepoints) used in the analysis.}
    \item{\code{cpts}:}{Object of class \code{"numeric"}, vector of online changepoints identified }
    \item{\code{ncpts.max}:}{Object of class \code{"numeric"}, maximum number of online changepoint that can be identified }
    \item{\code{param.est}:}{Object of class \code{"list"}, list where each element is a vector of parameter estimates, if requested }
    \item{\code{date}:}{Object of class \code{"character"}, date and time the online changepoint analysis was run }
    \item{\code{version}:}{Object of class \code{"character"}, version number of the package used when the analysis was run.}
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
    \item{cpts}{\code{signature(object = "ocpt.reg")}: retrieves cpts slot }
    \item{cpttype}{\code{signature(object = "ocpt.reg")}: retrieves cpttype slot }
    \item{sumstat}{\code{signature(object = "ocpt.reg")}: retrieves sumstat slot }
    \item{test.stat}{\code{signature(object = "ocpt.reg")}: retrieves test.stat slot }
    \item{ncpts.max}{\code{signature(object = "ocpt.reg")}: retrieves ncpts.max slot }
    \item{method}{\code{signature(object = "ocpt.reg")}: retrieves method slot }
    \item{minseglen}{\code{signature(object = "ocpt")}: retrieves minseglen slot }
    \item{param.est}{\code{signature(object = "ocpt.reg")}: retrieves param.est slot }
    \item{pen.type}{\code{signature(object = "ocpt.reg")}: retrieves pen.type slot }
    \item{pen.value}{\code{signature(object = "ocpt.reg")}: retrieves pen.value slot }
    \item{cpts<-}{\code{signature(object = "ocpt.reg")}: replaces cpts slot }
    \item{cpttype<-}{\code{signature(object = "ocpt.reg")}: replaces cpttype slot }
    \item{sumstat<-}{\code{signature(object = "ocpt.reg")}: replaces sumstat slot }
    \item{test.stat<-}{\code{signature(object = "ocpt.reg")}: replaces test.stat slot }
    \item{ncpts.max<-}{\code{signature(object = "ocpt.reg")}: replaces ncpts.max slot }
    \item{method<-}{\code{signature(object = "ocpt.reg")}: replaces method slot }
    \item{param.est<-}{\code{signature(object = "ocpt.reg")}: replaces param.est slot }
    \item{pen.type<-}{\code{signature(object = "ocpt.reg")}: replaces pen.type slot }
    \item{pen.value<-}{\code{signature(object = "ocpt.reg")}: replaces pen.value slot }
    \item{print}{\code{signature(object = "ocpt.reg")}: prints details of the cpt object including summary}
    \item{summary}{\code{signature(object = "ocpt.reg")}: prints a summary of the ocpt object }
    \item{param}{\code{signature(object = "ocpt.reg")}: calculates the parameter estimates for the ocpt object}
	 }
}
\author{
Andrew Connell
Rebecca Killick
}

\seealso{
\code{\link{plot-methods}},\code{\link{cpts-methods}},\code{\linkS4class{ocpt}}
}
\examples{
showClass("ocpt.reg")

x=new("ocpt.reg") # creates a new object with the cpt.reg class defaults
sumstat(x)  # retrieves the sumstat slot from x
sumstat(x)<-matrix(1:10,nrow=5,ncol=2) # replaces the sumstat slot from x with a matrix

}
\keyword{classes}
\keyword{ocpt}
\keyword{internal} 