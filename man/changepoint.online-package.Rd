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
Andrew Connell <a.connell1@lancaster.ac.uk>, Rebecca Killick <r.killick@lancs.ac.uk>, Matthew Nunes <m.nunes@lancs.ac.uk>.

Maintainer: Andrew Connell <a.connell1@lancaster.ac.uk>
}

\references{
PELT Algorithm: Killick R, Fearnhead P, Eckley IA (2012) Optimal detection of changepoints with a linear computational cost, \emph{JASA} \bold{107(500)}, 1590--1598

ECP test statistic: David's paper here

Online PELT: reference here.
}
\keyword{changepoint}
\keyword{segmentation}
\seealso{
\code{\link{ocpt.mean.initialise}},\code{\link{ocpt.mean.update}}\code{\link{ocpt.var.initialise}},\code{\link{ocpt.var.update}},\code{\link{ocpt.meanvar.initialise}},\code{\link{ocpt.meanvar.update}}
}
\examples{
#Please look in the desired function for examples.
}
