\name{ocpt.reg-class}
\Rdversion{1.1}
\docType{class}
\alias{ocpt.reg-class}
\alias{ocpt.reg-method,ocpts}
\alias{ocpt.reg-method,ocpttype}
\alias{ocpt.reg-method,data.set}
\alias{ocpt.reg-method,test.stat}
\alias{ocpt.reg-method,method}
\alias{ocpt.reg-method,nocpts.max}
\alias{ocpt.reg-method,param.est}
\alias{ocpt.reg-method,pen.type}
\alias{ocpt.reg-method,pen.value}
\alias{ocpt.reg-method,ocpts<-}
\alias{ocpt.reg-method,ocpttype<-}
\alias{ocpt.reg-method,data.set<-}
\alias{ocpt.reg-method,test.stat<-}
\alias{ocpt.reg-method,method<-}
\alias{ocpt.reg-method,nocpts.max<-}
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
    \item{\code{data.set}:}{Object of class \code{"numeric"}, the original vector of data }
    \item{\code{ocpttype}:}{Object of class \code{"character"}, the type of online changepoint that was identified }
    \item{\code{method}:}{Object of class \code{"character"}, the method that was used to search for online changepoints, default change in regression }
    \item{\code{test.stat}:}{Object of class \code{"character"}, the test statistic used to analyse the data }
    \item{\code{pen.type}:}{Object of class \code{"character"}, the penalty type specified in the analysis}
    \item{\code{pen.value}:}{Object of class \code{"numeric"}, the value of the penalty used in the analysis}
    \item{\code{minseglen}:}{Object of class \code{"numeric"}, the minimum segment length (no. of observations between online changepoints) used in the analysis.}
    \item{\code{ocpts}:}{Object of class \code{"numeric"}, vector of online changepoints identified }
    \item{\code{nocpts.max}:}{Object of class \code{"numeric"}, maximum number of online changepoint that can be identified }
    \item{\code{param.est}:}{Object of class \code{"list"}, list where each element is a vector of parameter estimates, if requested }
    \item{\code{date}:}{Object of class \code{"character"}, date and time the online changepoint analysis was run }
    \item{\code{version}:}{Object of class \code{"character"}, version number of the package used when the analysis was run.}
  }
}
\section{Methods}{
  \describe{
    \item{ocpts}{\code{signature(object = "ocpt.reg")}: retrieves ocpts slot }
    \item{ocpttype}{\code{signature(object = "ocpt.reg")}: retrieves ocpttype slot }
    \item{data.set}{\code{signature(object = "ocpt.reg")}: retrieves data.set slot }
    \item{test.stat}{\code{signature(object = "ocpt.reg")}: retrieves test.stat slot }
    \item{nocpts.max}{\code{signature(object = "ocpt.reg")}: retrieves nocpts.max slot }
    \item{method}{\code{signature(object = "ocpt.reg")}: retrieves method slot }
    \item{minseglen}{\code{signature(object = "ocpt")}: retrieves minseglen slot }
    \item{param.est}{\code{signature(object = "ocpt.reg")}: retrieves param.est slot }
    \item{pen.type}{\code{signature(object = "ocpt.reg")}: retrieves pen.type slot }
    \item{pen.value}{\code{signature(object = "ocpt.reg")}: retrieves pen.value slot }
    \item{ocpts<-}{\code{signature(object = "ocpt.reg")}: replaces ocpts slot }
    \item{ocpttype<-}{\code{signature(object = "ocpt.reg")}: replaces ocpttype slot }
    \item{data.set<-}{\code{signature(object = "ocpt.reg")}: replaces data.set slot }
    \item{test.stat<-}{\code{signature(object = "ocpt.reg")}: replaces test.stat slot }
    \item{nocpts.max<-}{\code{signature(object = "ocpt.reg")}: replaces nocpts.max slot }
    \item{method<-}{\code{signature(object = "ocpt.reg")}: replaces method slot }
    \item{param.est<-}{\code{signature(object = "ocpt.reg")}: replaces param.est slot }
    \item{pen.type<-}{\code{signature(object = "ocpt.reg")}: replaces pen.type slot }
    \item{pen.value<-}{\code{signature(object = "ocpt.reg")}: replaces pen.value slot }
    \item{print}{\code{signature(object = "ocpt.reg")}: prints details of the ocpt object including summary}
    \item{summary}{\code{signature(object = "ocpt.reg")}: prints a summary of the cpt object }
    \item{param}{\code{signature(object = "ocpt.reg")}: calculates the parameter estimates for the ocpt object}
	 }
}
\author{
Rebecca Killick
}

\seealso{
\code{\link{plot-methods}},\code{\link{ocpts-methods}},\code{\linkS4class{ocpt}}
}
\examples{
showClass("ocpt.reg")

x=new("ocpt.reg") # creates a new object with the cpt.reg class defaults
data.set(x)  # retrieves the data.set slot from x
data.set(x)<-matrix(1:10,nrow=5,ncol=2) # replaces the data.set slot from x with a matrix

}
\keyword{classes}
\keyword{ocpt}
\keyword{internal} 