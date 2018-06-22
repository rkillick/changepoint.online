\name{ocpt.plot}
\alias{ocpt.plot}
\title{
Plotting changes in Mean and variance. Update function.
}
\description{
Plots the optimal positioning of changepoints for data using the user specified method. 
}
\usage{
ocpt.plot(data, Q=4)
}
\arguments{
  \item{data}{
	The data that the user wants to plot. 
}
 \item{Q}{
	Maximum number of changepoints the plot will draw. Also twice this value is 
} 
}
\details{
	This function is used to plot the changes in mean and variance.
}
\value{
 Plots data.
}
\references{
Change in Normal mean: Hinkley, D. V. (1970) Inference About the Change-Point in a Sequence of Random Variables, \emph{Biometrika} \bold{57}, 1--17

PELT Algorithm: Killick R, Fearnhead P, Eckley IA (2012) Optimal detection of changepoints with a linear computational cost, \emph{JASA} \bold{107(500)}, 1590--1598

MBIC: Zhang, N. R. and Siegmund, D. O. (2007) A Modified Bayes Information Criterion with Applications to the Analysis of Comparative Genomic Hybridization Data. \emph{Biometrics} \bold{63}, 22-32.
}
\author{
Andrew Connell
Rebecca Killick
}


\seealso{
\code{\link{ocpt.var.initialise}},\code{\link{ocpt.mean.initiliase}},\code{\linkS4class{ocpt}}
}
\examples{
set.seed(1)
x=c(rnorm(50,0,1),rnorm(50,5,1),rnorm(50,10,1),rnorm(50,3,1))
ocpt.plot(x, Q=5)
}

\keyword{methods}
\keyword{univar}
\keyword{models}
\keyword{ts}
