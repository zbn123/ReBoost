setClassUnion("optlist", c("list","NULL"))

#' Class ''SMOTEBoost''
#'
#' \strong{SMOTEBoost} is a class of objects containing information that describes a SMOTEBoost model for standard and imbalanced regression tasks.
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
#' \item{\strong{For SMOTEBoost.R2}}{
#' \strong{niter}, the number of boosting iterations;
#' \strong{power} Exponent used to calculate the weight of each decision tree. Common values are 1 (linear), 2 (quadratic, default), 3 (cubic).
#' }
#' \item{\strong{For SMOTEBoost.BEM}}{
#' \strong{niter}, the number of boosting iterations;
#' \strong{BEM}, the Big Error Margin: a value that defines which errors (absolute distance) should be considered as being big.
#' }
#' \item{\strong{For SMOTEBoost.RT}}{
#' \strong{niter}, the number of boosting iterations;
#' \strong{thr}, absolute relative error threshold used to define prediction cases as hard to predict;
#' \strong{power}, exponent used to calculate the weight of each decision tree. Common values are 1 (linear), 2 (quadratic, default), 3 (cubic).
#' }
#' \item{\strong{For SMOTEBoost+}}{
#' \strong{niter}, the number of boosting iterations;
#' \strong{thr}, absolute relative error threshold used to define prediction cases as hard to predict;
#' \strong{power} Exponent used to calculate the weight of each decision tree. Common values are 1 (linear), 2 (quadratic, default), 3 (cubic);
#' \strong{sigma}, value used for regularization (default 0.5). If no regularization is expected, use NULL.
#' }
#' \item{\strong{For SMOTEBoost.RQ}}{
#' \strong{niter}, the number of boosting iterations;
#' \strong{power} Exponent used to calculate the weight of each decision tree. Common values are 1 (linear), 2 (quadratic, default), 3 (cubic).
#' }
#' }
#'
#' @slot perc_over Over-sampling percentage to apply to randomly selected cases with extreme values.
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
#'
#' @export
setClass("SMOTEBoost",
         slots = c(model = "list",
                   form = "formula",
                   model_pars = "optlist",
                   modeltype = "character",
                   perc_over = "numeric")
)

#' Creates SMOTEBoost models for regression tasks
#'
#' Creates a SMOTEBoost class model containing information that describes a SMOTEBoost model for standard and imbalanced regression tasks. In a nutshell, this method combines the application of the resampling strategy SMOTE in each round of boosting, in order to bias each decision tree towards a better representation of uncommon cases, i.e. with extreme target values.
#'
#' @param form A formula describing the prediction problem
#'
#' @param data A data frame containing the training data
#'
#' @param modeltype Type of SMOTEBoost method. This package implements 5 different proposals:
#' \describe{
#' \item{\strong{R2}}{SMOTEBoost.R2 using boosting method AdaBoost.R2 proposed by Drucker (1997).}
#' \item{\strong{BEM}}{SMOTBoost.BM using boosting method BEMBoost proposed by Feely (2000).}
#' \item{\strong{RT}}{SMOTEBoost.RT using boosting method AdaBoost.RT proposed by Solomatine and Shrestha (2004).}
#' \item{\strong{RTPlus}}{SMOTEBoost+ using boosting method AdaBoost+ proposed by Kankanala et al. (2014).}
#' \item{\strong{RQ}}{SMOTEBoost.RQ using boosting method AdaBoost.RQ proposed by package authors (publication in progress). Nutshell: uses the median of errors as a dynamic threshold.}
#' }
#'
#' @param model_pars A list with available parameters in creating decision trees and guiding the boosting process. Each boosting method allows for different parameters, which are detailed as follows:
#' \describe{
#' \item{\strong{For SMOTEBoost.R2}}{
#' \strong{niter}, the number of boosting iterations;
#' \strong{power} Exponent used to calculate the weight of each decision tree. Common values are 1 (linear), 2 (quadratic, default), 3 (cubic).
#' }
#' \item{\strong{For SMOTEBoost.BEM}}{
#' \strong{niter}, the number of boosting iterations;
#' \strong{BEM}, the Big Error Margin: a value that defines which errors (absolute distance) should be considered as being big.
#' }
#' \item{\strong{For SMOTEBoost.RT}}{
#' \strong{niter}, the number of boosting iterations;
#' \strong{thr}, absolute relative error threshold used to define prediction cases as hard to predict;
#' \strong{power}, exponent used to calculate the weight of each decision tree. Common values are 1 (linear), 2 (quadratic, default), 3 (cubic).
#' }
#' \item{\strong{For SMOTEBoost+}}{
#' \strong{niter}, the number of boosting iterations;
#' \strong{thr}, absolute relative error threshold used to define prediction cases as hard to predict;
#' \strong{power} Exponent used to calculate the weight of each decision tree. Common values are 1 (linear), 2 (quadratic, default), 3 (cubic);
#' \strong{sigma}, value used for regularization (default 0.5). If no regularization is expected, use NULL.
#' }
#' \item{\strong{For SMOTEBoost.RQ}}{
#' \strong{niter}, the number of boosting iterations;
#' \strong{power} Exponent used to calculate the weight of each decision tree. Common values are 1 (linear), 2 (quadratic, default), 3 (cubic).
#' }
#' }
#'
#' @param perc_over Over-sampling percentage to apply to randomly selected cases with extreme values, e.g. 1 for 100\% oversampling. Default is 0.8 (80\% oversampling).
#'
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
#' #For SMOTEBoost.RQ
#' m1 <- SMOTEBoost(form, tr, modeltype = "RQ", model_pars = NULL); predict(m1, ts)
#' m1p <- SMOTEBoost(form, tr, modeltype = "RQ", model_pars = list(niter=100,power=2), perc_over=1); predict(m1p, ts)
#'
#' #For SMOTEBoost.R2
#' m2 <- SMOTEBoost(form, tr, modeltype = "R2", model_pars = NULL); predict(m2, ts)
#' m2p <- SMOTEBoost(form, tr, modeltype = "R2", model_pars = list(niter=100,power=2), perc_over=1); predict(m2p, ts)
#'
#' #For SMOTEBoost.RT
#' m3 <- SMOTEBoost(form, tr, modeltype = "RT", model_pars = NULL); predict(m3, ts)
#' m3p <- SMOTEBoost(form, tr, modeltype = "RT", model_pars = list(niter=100,thr=0.1,power=2), perc_over=1); predict(m3p, ts)
#'
#' #For SMOTEBoost.RT+
#' m4 <- SMOTEBoost(form, tr, modeltype = "RTPlus", model_pars = NULL); predict(m4, ts)
#' m4p <- SMOTEBoost(form, tr, modeltype = "RTPlus", model_pars = list(niter=100,thr=0.1,power=2,sigma=0.5), perc_over=1); predict(m4p, ts)
#'
#' #For BEMBoost
#' m5 <- SMOTEBoost(form, tr, modeltype = "BEM", model_pars = NULL); predict(m5, ts)
#' m5p <- SMOTEBoost(form, tr, modeltype = "BEM", model_pars = list(niter=100,BEM=0.5), perc_over=1); predict(m5p, ts)
#'
SMOTEBoost <-
  function(form, data, modeltype="RQ", model_pars=NULL, perc_over=.8) {
    available_types <- c("RTPlus","RQ","R2","RT","BEM")

    if (!(modeltype %in% available_types)) {
      stop("The only available types of Boosting methods in this package are RTPlus, RQ, R2, RT and BEM.")
    }

    model <-
      switch(
        modeltype,
        "RTPlus" = {
          mpars <- get_pars("RTPlus", model_pars)

          SMOTEBoost.RTPlus.train(form = form,
                                  data = data,
                                  niter = mpars$niter,
                                  thr = mpars$thr,
                                  power = mpars$power,
                                  sigma = mpars$sigma,
                                  perc.O = perc_over)
        },
        "RQ" = {
          mpars <- get_pars("RQ", model_pars)

          SMOTEBoost.RQ.train(form = form,
                              data = data,
                              niter = mpars[["niter"]],
                              power = mpars$power,
                              perc.O = perc_over)
        },
        "R2" = {
          mpars <- get_pars("R2", model_pars)

          SMOTEBoost.R2.train(form = form,
                              data = data,
                              niter = mpars$niter,
                              power = mpars$power,
                              perc.O = perc_over)
        },
        "RT" = {
          mpars <- get_pars("RT", model_pars)

          SMOTEBoost.RT.train(form = form,
                              data = data,
                              niter=mpars$niter,
                              thr = mpars$thr,
                              power = mpars$power,
                              perc.O = perc_over)
        },
        "BEM" = {
          mpars <- get_pars("BEM", model_pars)

          SMOTEBoostBEM.train(form = form,
            data = data,
            niter=mpars$niter,
            BEM = mpars$BEM,
            perc.O = perc_over)
        }
      )

    methods::new(
      "SMOTEBoost",
      model = model$models,
      form = form,
      model_pars = model$model_pars,
      modeltype = modeltype,
      perc_over = perc_over)
  }


setMethod("show",
          signature("SMOTEBoost"),
          function(object) {
            cat("### Model Type: SMOTEBoost.",object@modeltype, "\n\n")
            cat(length(object@model)," rounds of boosting with",object@perc_over * 100,"% of SMOTE on cases with extreme values.\n")
            cat("Individual decision trees can be accessed in slot 'model'.\n\n")
          }
)


get_pars <-
  function(modeltype, model_pars) {
    switch(
      modeltype,
      "RTPlus" = {
        if (is.null(model_pars)) {
          return(list(
            niter = 100,
            thr = .01,
            power = 2,
            sigma = .5
          ))
        }

        if (is.null(model_pars[["niter"]])) {
          niter <- 100
          return(c(model_pars, niter = niter))
        }

        if (is.null(model_pars[["thr"]])) {
          thr <- .01
          return(c(model_pars, thr = thr))
        }

        if (is.null(model_pars[["power"]])) {
          power <- 2
          return(c(model_pars, power = power))
        }

        if (is.null(model_pars[["sigma"]])) {
          sigma <- .5
          return(c(model_pars, sigma = sigma))
        }
      },
      "RQ" = {

        print(model_pars)

        if (is.null(model_pars)) {
          return(list(niter = 100, power = 2))
        }

        if (is.null(model_pars[["niter"]])) {
          niter <- 100
          return(c(model_pars, niter = niter))
        }

        if (is.null(model_pars[["power"]])) {
          power <- 2
          return(c(model_pars, power = power))
        }

      },
      "R2" = {
        if (is.null(model_pars)) {
          return(list(
            niter = 100,
            power = 2
          ))
        }

        if (is.null(model_pars[["niter"]])) {
          niter <- 100
          return(c(model_pars, niter = niter))
        }

        if (is.null(model_pars[["power"]])) {
          power <- 2
          return(c(model_pars, power = power))
        }

      },
      "RT" = {
        if (is.null(model_pars)) {
          return(list(
            niter = 100,
            thr = .01,
            power = 2,
            sigma = .5
          ))
        }

        if (is.null(model_pars[["niter"]])) {
          niter <- 100
          return(c(model_pars, niter = niter))
        }

        if (is.null(model_pars[["thr"]])) {
          thr <- .01
          return(c(model_pars, thr = thr))
        }

        if (is.null(model_pars[["power"]])) {
          power <- 2
          return(c(model_pars, power = power))
        }

        if (is.null(model_pars[["sigma"]])) {
          sigma <- .5
          return(c(model_pars, sigma = sigma))
        }

      },
      "BEM" = {
        if (is.null(model_pars)) {
          return(list(
            niter = 100,
            BEM = 0.5
          ))
        }

        if (is.null(model_pars[["niter"]])) {
          niter <- 100
          return(c(model_pars, niter = niter))
        }

        if (is.null(model_pars[["BEM"]])) {
          BEM <- 0.5
          return(c(model_pars, BEM = BEM))
        }

      }
    )
  }


#' Predict method for objects of SMOTEBoost class
#'
#' @param object A SMOTEBoost object
#' @param newdata A data frame containing the test data
#'
#' @examples
#' library(ReBoost)
#' data(iris)
#' ind <- sample(0.75*nrow(iris))
#' tr <- iris[ind,]
#' ts <- iris[-ind,]
#' form<-Sepal.Length ~ .
#'
#' #For SMOTEBoost.RQ
#' m1 <- SMOTEBoost(form, tr, modeltype = "RQ", model_pars = NULL); predict(m1, ts)
#' m1p <- SMOTEBoost(form, tr, modeltype = "RQ", model_pars = list(niter=100,power=2), perc_over=1); predict(m1p, ts)
#'
#' @export
setMethod("predict",
          signature("SMOTEBoost"),
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
