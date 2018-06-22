\name{changepoint.online-package}
\alias{changepoint.online-package}
\alias{changepoint.online}
\docType{package}
\title{
Methods for online Changepoint Detection
}
\description{
Implements various mainstream and specialised changepoint methods for finding single and multiple changepoints within data.  Many popular non-parametric and frequentist methods are included.  Users should start by looking at the documentation for ocpt.mean.initialise() and ocpt.mean.update().
}
\details{
\tabular{ll}{
Package: \tab changepoint.online\cr
Type: \tab Package\cr
Version: \tab 0.0.1 \cr
Date: \tab 2018-06-25\cr
License: \tab GPL\cr
LazyLoad: \tab yes\cr
}

}
\author{
Andrew Connell <a.connell1@lancaster.ac.uk>, Rebecca Killick <r.killick@lancs.ac.uk>, Kaylea Haynes <k.haynes1@lancs.ac.uk> with contributions from Idris A. Eckley <i.eckley@lancs.ac.uk>, Paul Fearnhead <p.fearnhead@lancs.ac.uk>.

Maintainer: Rebecca Killick <r.killick@lancs.ac.uk>, Andrew Connell <a.connell1@lancaster.ac.uk>
}

\references{
Chen, J. and Gupta, A. K. (2000) \emph{Parametric statistical change point analysis}, Birkhauser

PELT Algorithm: Killick R, Fearnhead P, Eckley IA (2012) Optimal detection of changepoints with a linear computational cost, \emph{JASA} \bold{107(500)}, 1590--1598

Binary Segmentation: Scott, A. J. and Knott, M. (1974) A Cluster Analysis Method for Grouping Means in the Analysis of Variance, \emph{Biometrics} \bold{30(3)}, 507--512

Segment Neighbourhoods: Auger, I. E. And Lawrence, C. E. (1989) Algorithms for the Optimal Identification of Segment Neighborhoods, \emph{Bulletin of Mathematical Biology} \bold{51(1)}, 39--54

}
\keyword{changepoint}
\keyword{segmentation}
\seealso{
\code{\link{ocpt.mean.initialise}},\code{\link{ocpt.mean.update}}\code{\link{ocpt.var.initialise}},\code{\link{ocpt.var.update}},\code{\link{ocpt.meanvar.initialise}},\code{\link{ocpt.meanvar.update}}
}
\examples{
#Please look in the desired function for examples.
}
