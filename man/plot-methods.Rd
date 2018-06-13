\name{plot-methods}
\docType{methods}
\alias{plot-methods}
\alias{plot,ANY-method}
\alias{plot,ocpt-method}
\alias{plot,ocpt.range-method}
\alias{plot,ocpt.reg-method}
\title{ ~~ Methods for Function plot in Package `graphics' ~~}
\description{
 ~~ Methods for function \code{plot} in Package `graphics' ~~
}
\section{Methods}{
\describe{

\item{\code{signature(x = "ANY")}}{
	Generic plot function, see graphics package description using ?plot
}

\item{\code{signature(x = "ocpt")}}{
	Plots the data and identifies the changepoints using vertical lines (change in variance), horizontal lines (change in mean).  Optional arguments to control the lines: \code{ocpt.col} equivilent to \code{col} to change the colour of the changepoint line; \code{ocpt.width} equivilent to \code{lwd} to change the width of the changepoint line; \code{ocpt.style} equivilent to \code{lty} to change the style of the line.
}
\item{\code{signature(x = "ocpt.range")}}{
	As for the \code{ocpt} objects except for two optional arguments, \code{nocpts} and \code{diagnostic}.  The \code{nocpts} option allows you to specify a plot of the segmentation with \code{nocpts} changepoints in, i.e. the optimal may be specified as 10 changes but you want to plot the segmentation with 5 changes (provided a segmentation with 5 changes is listed in \code{ocpts.full(x)}.  The \code{diagnostic} option when set to \code{TRUE} plots the number of changepoints in each segmentation against the change in test statistic when adding that change.  This can aide the decision on the number of changepoints as when a true changepoint is added the cost increases/decreases rapidly, but when a changepoint due to noise is added the change is small.  This is akin to a scree plot in principal component analysis.  The idea is that someone may choose to create a plot using \code{diagnostic=TRUE}, identify the appropriate number of changes and then replot using \code{nocpts} to visualize that segmentation.
}
\item{\code{signature(x = "ocpt.reg")}}{
	Plotting is only valid for one regressor.  Plots the regressor against the response and identifies the changepoints using horizontal lines.  Optional arguments to control the lines: \code{ocpt.col} equivilent to \code{col} to change the colour of the changepoint line; \code{ocpt.width} equivilent to \code{lwd} to change the width of the changepoint line; \code{ocpt.style} equivilent to \code{lty} to change the style of the line.
}

}}
\keyword{methods}
\keyword{plot}
\keyword{ocpt}
\keyword{internal} 