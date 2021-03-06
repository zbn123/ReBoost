% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bemboost.r
\name{BEMBoost.train}
\alias{BEMBoost.train}
\title{Create a BEMBoost model}
\usage{
BEMBoost.train(form, data, niter = 100, BEM = 0.5)
}
\arguments{
\item{form}{A formula describing the prediction problem}

\item{data}{A data frame containing the training data}

\item{niter}{The number of boosting iterations}

\item{BEM}{Big Error Margin. A value that defines which errors (absolute distance) should be considered as being big.}
}
\description{
Create a BEMBoost model
}
\keyword{internal}
