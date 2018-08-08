% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/adaboost-class.r
\name{AdaBoost}
\alias{AdaBoost}
\title{AdaBoost}
\usage{
AdaBoost(form, data, modeltype, model_pars)
}
\arguments{
\item{form}{A formula describing the prediction problem}

\item{data}{A data frame containing the training data}

\item{modeltype}{Type of boosting method}

\item{model_pars}{Parameters used to create decision trees and guide the boosting process}
}
\description{
\strong{AdaBoost} is a S4 class that contains
}
\examples{
library(ReBoost)
data(iris)
ind <- sample(0.75*nrow(iris))
tr <- iris[ind,]
ts <- iris[-ind,]
form<-Sepal.Length ~ .

#For AdaBoost.RQ
m1 <- AdaBoost(form, tr, modeltype = "RQ", model_pars = NULL); predict(m1, ts)

#For AdaBoost.R2
m2 <- AdaBoost(form, iris, modeltype = "R2", model_pars = NULL); predict(m2, ts)

#For AdaBoost.RT
m3 <- AdaBoost(form, iris, modeltype = "RT", model_pars = NULL); predict(m3, ts)

#For AdaBoost.RT+
m4 <- AdaBoost(form, iris, modeltype = "RTPlus", model_pars = NULL); predict(m4, ts)

#For BEMBoost
m5 <- AdaBoost(form, iris, modeltype = "BEM", model_pars = NULL); predict(m5, ts)

}