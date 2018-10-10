setClassUnion("optlist", c("list","NULL"))

#' Class ''AdaBoost''
#'
#' \strong{AdaBoost} is a class of objects containing information that describes a boosting model for regression tasks.
#'
#' @slot model A set of decision trees corresponding to the number of boosting iterations
#'
#' @slot form A formula describing the prediction problem
#'
#' @slot modeltype Type of boosting method. This packages implements 5 different proposals:
#' \describe{
#' \item{\strong{R2}}{AdaBoost.R2 proposed by Drucker (1997).}
#' \item{\strong{BEM}}{BEMBoost proposed by Feely (2000).}
#' \item{\strong{RT}}{AdaBoost.RT proposed by Solomatine and Shrestha (2004).}
#' \item{\strong{RTPlus}}{AdaBoost+ proposed by Kankanala et al. (2014).}
#' \item{\strong{RQ}}{AdaBoost.RQ proposed by package authors (publication in progress). Nutshell: uses the median of errors as a dynamic threshold.}
#' }
#'
#' @slot model_pars A list with available parameters in creating decision trees and guiding the boosting process. Each boosting method allows for different parameters, which are detailed as follows:
#' \describe{
#' \item{\strong{For AdaBoost.R2}}{
#' \strong{niter}, the number of boosting iterations;
#' \strong{power} Exponent used to calculate the weight of each decision tree. Common values are 1 (linear), 2 (quadratic, default), 3 (cubic).
#' }
#' \item{\strong{For BEMBoost}}{
#' \strong{niter}, the number of boosting iterations;
#' \strong{BEM}, the Big Error Margin: a value that defines which errors (absolute distance) should be considered as being big.
#' }
#' \item{\strong{For AdaBoost.RT}}{
#' \strong{niter}, the number of boosting iterations;
#' \strong{thr}, absolute relative error threshold used to define prediction cases as hard to predict;
#' \strong{power}, exponent used to calculate the weight of each decision tree. Common values are 1 (linear), 2 (quadratic, default), 3 (cubic).
#' }
#' \item{\strong{For AdaBoost+}}{
#' \strong{niter}, the number of boosting iterations;
#' \strong{thr}, absolute relative error threshold used to define prediction cases as hard to predict;
#' \strong{power} Exponent used to calculate the weight of each decision tree. Common values are 1 (linear), 2 (quadratic, default), 3 (cubic);
#' \strong{sigma}, value used for regularization (default 0.5). If no regularization is expected, use NULL.
#' }
#' \item{\strong{For AdaBoost.RQ}}{
#' \strong{niter}, the number of boosting iterations;
#' \strong{power} Exponent used to calculate the weight of each decision tree. Common values are 1 (linear), 2 (quadratic, default), 3 (cubic).
#' }
#' }
#'
#' @keywords internal
#'
#' @references
#' N. Moniz, R. P. Ribeiro, V. Cerqueira, N. Chawla (2018). "SMOTEBoost for Regression: Improving the Prediction of Extreme Values", in Proceedings of the 5th IEEE International Conference on Data Science and Advanced Analytics, IEEE Xplore. 2018.
#'
#'
#' H. Drucker, “Improving regressors using boosting techniques”, in Proceedings of the Fourteenth International Conference on Machine Learning, ICML’97, Morgan Kaufmann Publishers Inc., pp. 107–115. 1997.
#'
#'
#' R. Feely, “Predicting stock market volatility using neural networks", Ph.D. Dissertation, Trinity College, Dublin, 2000.
#'
#'
#' D. P. Solomatine and D. L. Shrestha, “Adaboost.rt: a boosting algorithm for regression problems,” in 2004 IEEE International Joint Conference on Neural Networks, vol. 2, pp. 1163–1168 vol.2, 2004.
#'
#' P. Kankanala, S. Das, and A. Pahwa, “Adaboost+: An ensemble learning approach for estimating weather-related outages in distribution systems", IEEE Transactions on Power Systems, vol. 29, no. 1, pp. 359–367, 2014.
#'
#' @export
setClass("AdaBoost",
         slots = c(model = "list",
                   form = "formula",
                   modeltype = "character",
                   model_pars = "optlist")
)

#' Create AdaBoost models for regression tasks
#'
#' Creates an AdaBoost class model containing information that describes a boosting model for regression tasks.
#'
#' @param form A formula describing the prediction problem
#'
#' @param data A data frame containing the training data
#'
#' @param modeltype Type of boosting method. This package implements 5 different proposals:
#' \describe{
#' \item{\strong{R2}}{AdaBoost.R2 proposed by Drucker (1997).}
#' \item{\strong{BEM}}{BEMBoost proposed by Feely (2000).}
#' \item{\strong{RT}}{AdaBoost.RT proposed by Solomatine and Shrestha (2004).}
#' \item{\strong{RTPlus}}{AdaBoost+ proposed by Kankanala et al. (2014).}
#' \item{\strong{RQ}}{AdaBoost.RQ proposed by package authors (publication in progress). Nutshell: uses the median of errors as a dynamic threshold.}
#' }
#'
#' @param model_pars A list with available parameters in creating decision trees and guiding the boosting process. Each boosting method allows for different parameters, which are detailed as follows:
#' \describe{
#' \item{\strong{For AdaBoost.R2}}{
#' \strong{niter}, the number of boosting iterations;
#' \strong{power} Exponent used to calculate the weight of each decision tree. Common values are 1 (linear), 2 (quadratic, default), 3 (cubic).
#' }
#' \item{\strong{For BEMBoost}}{
#' \strong{niter}, the number of boosting iterations;
#' \strong{BEM}, the Big Error Margin: a value that defines which errors (absolute distance) should be considered as being big.
#' }
#' \item{\strong{For AdaBoost.RT}}{
#' \strong{niter}, the number of boosting iterations;
#' \strong{thr}, absolute relative error threshold used to define prediction cases as hard to predict;
#' \strong{power}, exponent used to calculate the weight of each decision tree. Common values are 1 (linear), 2 (quadratic, default), 3 (cubic).
#' }
#' \item{\strong{For AdaBoost+}}{
#' \strong{niter}, the number of boosting iterations;
#' \strong{thr}, absolute relative error threshold used to define prediction cases as hard to predict;
#' \strong{power} Exponent used to calculate the weight of each decision tree. Common values are 1 (linear), 2 (quadratic, default), 3 (cubic);
#' \strong{sigma}, value used for regularization (default 0.5). If no regularization is expected, use NULL.
#' }
#' \item{\strong{For AdaBoost.RQ}}{
#' \strong{niter}, the number of boosting iterations;
#' \strong{power} Exponent used to calculate the weight of each decision tree. Common values are 1 (linear), 2 (quadratic, default), 3 (cubic).
#' }
#' }
#'
#' @keywords internal
#'
#' @references
#' N. Moniz, R. P. Ribeiro, V. Cerqueira, N. Chawla (2018). "SMOTEBoost for Regression: Improving the Prediction of Extreme Values", in Proceedings of the 5th IEEE International Conference on Data Science and Advanced Analytics, IEEE Xplore. 2018.
#'
#'
#' H. Drucker, “Improving regressors using boosting techniques”, in Proceedings of the Fourteenth International Conference on Machine Learning, ICML’97, Morgan Kaufmann Publishers Inc., pp. 107–115. 1997.
#'
#'
#' R. Feely, “Predicting stock market volatility using neural networks", Ph.D. Dissertation, Trinity College, Dublin, 2000.
#'
#'
#' D. P. Solomatine and D. L. Shrestha, “Adaboost.rt: a boosting algorithm for regression problems,” in 2004 IEEE International Joint Conference on Neural Networks, vol. 2, pp. 1163–1168 vol.2, 2004.
#'
#' P. Kankanala, S. Das, and A. Pahwa, “Adaboost+: An ensemble learning approach for estimating weather-related outages in distribution systems", IEEE Transactions on Power Systems, vol. 29, no. 1, pp. 359–367, 2014.
#'
#' @export
#'
#' @examples
#' library(ReBoost)
#' data(iris)
#' ind <- sample(0.75*nrow(iris))
#' tr <- iris[ind,]
#' ts <- iris[-ind,]
#' form<-Sepal.Length ~ .
#'
#' #For AdaBoost.RQ
#' m1 <- AdaBoost(form, tr, modeltype = "RQ", model_pars = NULL); predict(m1, ts)
#' m1p <- AdaBoost(form, tr, modeltype = "RQ", model_pars = list(niter=100,power=2)); predict(m1p, ts)
#'
#' #For AdaBoost.R2
#' m2 <- AdaBoost(form, tr, modeltype = "R2", model_pars = NULL); predict(m2, ts)
#' m2p <- AdaBoost(form, tr, modeltype = "R2", model_pars = list(niter=100,power=2)); predict(m2p, ts)
#'
#' #For AdaBoost.RT
#' m3 <- AdaBoost(form, tr, modeltype = "RT", model_pars = NULL); predict(m3, ts)
#' m3p <- AdaBoost(form, tr, modeltype = "RT", model_pars = list(niter=100,thr=0.1,power=2)); predict(m3p, ts)
#'
#' #For AdaBoost.RT+
#' m4 <- AdaBoost(form, tr, modeltype = "RTPlus", model_pars = NULL); predict(m4, ts)
#' m4p <- AdaBoost(form, tr, modeltype = "RTPlus", model_pars = list(niter=100,thr=0.1,power=2,sigma=0.5)); predict(m4p, ts)
#'
#' #For BEMBoost
#' m5 <- AdaBoost(form, tr, modeltype = "BEM", model_pars = NULL); predict(m5, ts)
#' m5p <- AdaBoost(form, tr, modeltype = "BEM", model_pars = list(niter=100,BEM=0.5)); predict(m5p, ts)
#'
AdaBoost <-
  function(form, data, modeltype="RQ", model_pars=NULL) {
    available_types <- c("RTPlus","RQ","R2","RT","BEM")

    if (!(modeltype %in% available_types)) {
      stop("The only available types of Boosting methods in this package are RTPlus, RQ, R2, RT and BEM.")
    }

    model <-
      switch(
        modeltype,
        "RTPlus" = {
          mpars <- get_pars("RTPlus", model_pars)

          AdaBoost.RTPlus.train(
            form = form,
            data = data,
            niter = mpars$niter,
            thr = mpars$thr,
            power = mpars$power,
            sigma = mpars$sigma)
        },
        "RQ" = {
          mpars <- get_pars("RQ", model_pars)

          AdaBoost.RQ.train(form = form,
                              data = data,
                              niter = mpars$niter,
                              power = mpars$power)
        },
        "R2" = {
          mpars <- get_pars("R2", model_pars)

          AdaBoost.R2.train(form = form,
                              data = data,
                              niter = mpars$niter,
                              power = mpars$power)
        },
        "RT" = {
          mpars <- get_pars("RT", model_pars)

          AdaBoost.RT.train(form = form,
                              data = data,
                              niter=mpars$niter,
                              thr = mpars$thr,
                              power = mpars$power)
        },
        "BEM" = {
          mpars <- get_pars("BEM", model_pars)

          BEMBoost.train(form = form,
            data = data,
            niter=mpars$niter,
            BEM=mpars$BEM)
        }
      )

    methods::new(
      "AdaBoost",
      model = model$models,
      form = form,
      model_pars = model$model_pars,
      modeltype = modeltype)
  }


setMethod("show",
          signature("AdaBoost"),
          function(object) {
            cat("### Model Type: AdaBoost.",object@modeltype, "\n\n")
            cat(length(object@model)," rounds of boosting.\n")
            cat("Individual decision trees can be accessed in slot 'model'.\n\n")
          }
)


#' Predict method for objects of AdaBoost class
#'
#' @description Method for prediction when using objects of the class AdaBoost.
#'
#' @param object An AdaBoost object
#' @param newdata A data frame containing the test data
#'
#' @export
#'
#' @examples
#' library(ReBoost)
#' data(iris)
#' ind <- sample(0.75*nrow(iris))
#' tr <- iris[ind,]
#' ts <- iris[-ind,]
#' form<-Sepal.Length ~ .
#'
#' #For AdaBoost.RQ
#' m1 <- AdaBoost(form, tr, modeltype = "RQ", model_pars = NULL); predict(m1, ts)
#' m1p <- AdaBoost(form, tr, modeltype = "RQ", model_pars = list(niter=100,power=2)); predict(m1p, ts)
#'
setMethod("predict",
          signature("AdaBoost"),
          function(object, newdata) {
            y_hat <-
              switch(
                object@modeltype,
                "RQ" = {
                  AdaBoost.RQ.predict(models = object@model,
                                      betas = object@model_pars$beta,
                                      newdata = newdata)
                },
                "RTPlus" = {
                  AdaBoost.RTPlus.predict(
                    models = object@model,
                    newdata = newdata,
                    delta = object@model_pars$delta)
                },
                "R2" = {
                  AdaBoost.R2.predict(models = object@model,
                                      betas = object@model_pars$beta,
                                      newdata = newdata)
                },
                "RT" = {
                  AdaBoost.RT.predict(models = object@model,
                                      newdata = newdata,
                                      betas = object@model_pars$beta)
                },
                "BEM" = {
                  BEMBoost.predict(models = object@model,
                                   newdata = newdata)
                }
              )

            y_hat
          })
