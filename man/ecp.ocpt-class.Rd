\name{ecp.ocpt-class}
\Rdversion{1.1}
\docType{class}
\alias{ecp.ocpt-class}
\alias{ecp.ocpt-method,number}
\alias{ecp.ocpt-method,estimates}
\alias{ecp.ocpt-method,GofM}
\alias{ecp.ocpt-method,delta}
\alias{ecp.ocpt-method,alpha}
\alias{ecp.ocpt-method,verbose}
\alias{ecp.ocpt-method,csum}
\alias{ecp.ocpt-method,dll}
\alias{ecp.ocpt-method,dlr}
\alias{ecp.ocpt-method,drr}
\alias{ecp.ocpt-method,left}
\alias{ecp.ocpt-method,right}
\alias{ecp.ocpt-method,datalength}
\alias{ecp.ocpt-method,functime}
\alias{ecp.ocpt-method,width}
\alias{ecp.ocpt-method,cpLoc}
\alias{ecp.ocpt-method,number<-}
\alias{ecp.ocpt-method,estimates<-}
\alias{ecp.ocpt-method,GofM<-}
\alias{ecp.ocpt-method,delta<-}
\alias{ecp.ocpt-method,alpha<-}
\alias{ecp.ocpt-method,verbose<-}
\alias{ecp.ocpt-method,csum<-}
\alias{ecp.ocpt-method,dll<-}
\alias{ecp.ocpt-method,dlr<-}
\alias{ecp.ocpt-method,drr<-}
\alias{ecp.ocpt-method,left<-}
\alias{ecp.ocpt-method,right<-}
\alias{ecp.ocpt-method,datalength<-}
\alias{ecp.ocpt-method,functime<-}
\alias{ecp.ocpt-method,width<-}
\alias{ecp.ocpt-method,cpLoc<-}
\alias{ocpt.ocpt-method,summary}

\title{Class "ecp.ocpt"}
\description{
A class for online changepoint objects for the test.stat="ECP" case.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("ecp.ocpt", ...)}.
  \describe{
    \item{\code{new("ecp.ocpt", ...)}:}{creates a new object with class ecp.ocpt }
  }
}
\section{Slots}{
  \describe{
    \item{\code{number}:}{The estimated number of change points.}
    \item{\code{estimates}:}{The location of the change points estimated by the procedure.}
    \item{\code{GofM}:}{Goodness of fit Model.}
    \item{\code{delta}:}{The window size used to calculate the calculate the complete portion of our approximate test statistic. This also corresponds to one less than the minimum segment size. It is the same as minseglen for PELT.}
    \item{\code{alpha}:}{The moment index used for determining the distance between and within segments.}
    \item{\code{verbose}:}{A flag indicating if status updates should be printed.}
    \item{\code{csum}:}{The cumulative distance sum of all datapoints.}
    \item{\code{dll}:}{Sum of within sample distances for left segments.}
    \item{\code{dlr}:}{Between sample distance sum using dll and drr.}
    \item{\code{drr}:}{Sum of within sample distances for right segments.}
    \item{\code{left}:}{A vector which gives the sum of distances before point i at Left(i,0). Left(i,1) is to resolve out of bound errors.}
    \item{\code{right}:}{A vector which gives the sum of distances after point i at Right(i,0). Right(i,1) is to resolve out of bound errors.}
    \item{\code{datalength}:}{This is the total length of the observations.}
    \item{\code{functime}:}{The total amount to time take to estimate the change point locations.}
    \item{\code{width}:}{This is the number of changepoints located in the dataset.}
    \item{\code{cpLoc}:}{The list of locations of change points estimated by the procedure for different numbers of change points up to K (the maximum number of changepoints the user wishes to search for).}
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
    \item{ecp.ocpts}{\code{signature(object = "ecp.ocpt")}: retrieves ecp.ocpts slot }
    \item{number}{\code{signature(object = "ecp.ocpt")}: retrieves number slot }
    \item{estimates}{\code{signature(object = "ecp.ocpt")}: retrieves estimates slot }
    \item{GofM}{\code{signature(object = "ecp.ocpt")}: retrieves GofM slot }
    \item{delta}{\code{signature(object = "ecp.ocpt")}: retrieves delta slot }
    \item{alpha}{\code{signature(object = "ecp.ocpt")}: retrieves alpha slot }
    \item{verbose}{\code{signature(object = "ecp.ocpt")}: retrieves verbose slot }
    \item{csum}{\code{signature(object = "ecp.ocpt")}: retrieves csum slot }
    \item{dll}{\code{signature(object = "ecp.ocpt")}: retrieves dll slot }
    \item{dlr}{\code{signature(object = "ecp.ocpt")}: retrieves dlr slot }
    \item{drr}{\code{signature(object = "ecp.ocpt")}: retrieves drr slot }
    \item{left}{\code{signature(object = "ecp.ocpt")}: retrieves left slot }
    \item{right}{\code{signature(object = "ecp.ocpt")}: retrieves right slot }
    \item{datalength}{\code{signature(object = "ecp.ocpt")}: retrieves datalength slot }
    \item{functime}{\code{signature(object = "ecp.ocpt")}: retrieves time slot }
    \item{width}{\code{signature(object = "ecp.ocpt")}: retrieves width slot }
    \item{cpLoc}{\code{signature(object = "ecp.ocpt")}: retrieves csum slot }
    \item{number<-}{\code{signature(object = "ecp.ocpt")}: replaces number slot }
    \item{estimates<-}{\code{signature(object = "ecp.ocpt")}: replaces estimates slot }
    \item{GofM<-}{\code{signature(object = "ecp.ocpt")}: replaces GofM slot }
    \item{delta<-}{\code{signature(object = "ecp.ocpt")}: replaces delta slot }
    \item{alpha<-}{\code{signature(object = "ecp.ocpt")}: replaces alpha slot }
    \item{verbose<-}{\code{signature(object = "ecp.ocpt")}: replaces verbose slot }
    \item{csum<-}{\code{signature(object = "ecp.ocpt")}: replaces csum slot }
    \item{dll<-}{\code{signature(object = "ecp.ocpt")}: replaces dll slot }
    \item{dlr<-}{\code{signature(object = "ecp.ocpt")}: replaces dlr slot }
    \item{drr<-}{\code{signature(object = "ecp.ocpt")}: replaces drr slot }
    \item{left<-}{\code{signature(object = "ecp.ocpt")}: replaces left slot }
    \item{right<-}{\code{signature(object = "ecp.ocpt")}: replaces right slot }
    \item{datalength<-}{\code{signature(object = "ecp.ocpt")}: replaces datalength slot }
    \item{functime<-}{\code{signature(object = "ecp.ocpt")}: replaces time slot }
    \item{width<-}{\code{signature(object = "ecp.ocpt")}: replaces width slot }
    \item{cpLoc<-}{\code{signature(object = "ecp.ocpt")}: replaces cpLoc slot }
    \item{summary}{\code{signature(object = "ecp.ocpt")}: prints a summary of the ecp.ocpt object }
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
Andrew Connell, Rebecca Killick, David Matteson
}

\seealso{
\code{\link{ocpt.np.initialise}},\code{\link{ocpt.mean.initialise}},\code{\link{ocpt.var.initialise}},\code{\link{ocpt.meanvar.initialise}}
}
\examples{
showClass("ecp.ocpt") # shows the structure of the ecp.ocpt class

x=new("ecp.ocpt") # creates a new object with the ecp.ocpt class defaults
estimates(x) # retrieves the ecp.ocpts slot from x

# Example of a change in variance at 100 in simulated normal data
set.seed(1)
x=matrix(c(rnorm(100,0,1),rnorm(100,0,10)),ncol=1)
ans=ocpt.np.initialise(x) 
print(ans) # prints details of the analysis including a summary
summary(ans)
}
\keyword{classes}
\keyword{ecp.ocpt}
\keyword{internal} 