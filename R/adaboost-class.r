setClassUnion("optlist", c("list","NULL"))

#' AdaBoost-class
#'
#' \strong{AdaBoost} is a S4 class that contains
#'
#' @slot model A set of decision trees corresponding to the number of boosting iterations
#'
#' @slot form A formula describing the prediction problem
#'
#' @slot model_pars Parameters used to create decision trees and guide the boosting process
#'
#' @slot modeltype Type of boosting method
#'
#' #' This package enables the implementation of 5 different proposals for AdaBoost including those based on AdaBoost.R2 (Drucker1997), AdaBoost.RQ, AdaBoost.RT (Solomatine2004), AdaBoost.RT+ (Kankanala2014) and BEMBoost (Feely2000).
#'
#' @keywords internal
#'
#' @references
#' H. Drucker, “Improving regressors using boosting techniques”, in Proceedings of the Fourteenth International Conference on Machine Learning, ICML’97, Morgan Kaufmann Publishers Inc., pp. 107–115. 1997.
#' R. Feely, “Predicting stock market volatility using neural networks", Ph.D. Dissertation, Trinity College, Dublin, 2000.
#' D. P. Solomatine and D. L. Shrestha, “Adaboost.rt: a boosting algorithm for regression problems,” in 2004 IEEE International Joint Conference on Neural Networks, vol. 2, pp. 1163–1168 vol.2, 2004.
#' P. Kankanala, S. Das, and A. Pahwa, “Adaboost+: An ensemble learning approach for estimating weather-related outages in distribution systems", IEEE Transactions on Power Systems, vol. 29, no. 1, pp. 359–367, 2014.
#'
#' @export
setClass("AdaBoost",
         slots = c(model = "list",
                   form = "formula",
                   model_pars = "optlist",
                   modeltype = "character")
)

#' AdaBoost
#'
#' \strong{AdaBoost} is a S4 class that contains
#'
#' @param form A formula describing the prediction problem
#'
#' @param data A data frame containing the training data
#'
#' @param modeltype Type of boosting method
#'
#' @param model_pars Parameters used to create decision trees and guide the boosting process
#'
#' This package enables the implementation of 5 different proposals for AdaBoost including those based on AdaBoost.R2 (Drucker1997), AdaBoost.RQ, AdaBoost.RT (Solomatine2004), AdaBoost.RT+ (Kankanala2014) and BEMBoost (Feely2000).
#'
#' @keywords internal
#'
#' @references
#' N. Moniz, R. P. Ribeiro, V. Cerqueira, N. Chawla (2018). "SMOTEBoost for Regression: Improving the Prediction of Extreme Values", in Proceedings of the 5th IEEE International Conference on Data Science and Advanced Analytics, IEEE Xplore. 2018.
#' H. Drucker, “Improving regressors using boosting techniques”, in Proceedings of the Fourteenth International Conference on Machine Learning, ICML’97, Morgan Kaufmann Publishers Inc., pp. 107–115. 1997.
#' R. Feely, “Predicting stock market volatility using neural networks", Ph.D. Dissertation, Trinity College, Dublin, 2000.
#' D. P. Solomatine and D. L. Shrestha, “Adaboost.rt: a boosting algorithm for regression problems,” in 2004 IEEE International Joint Conference on Neural Networks, vol. 2, pp. 1163–1168 vol.2, 2004.
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
#'
#' #For AdaBoost.R2
#' m2 <- AdaBoost(form, iris, modeltype = "R2", model_pars = NULL); predict(m2, ts)
#'
#' #For AdaBoost.RT
#' m3 <- AdaBoost(form, iris, modeltype = "RT", model_pars = NULL); predict(m3, ts)
#'
#' #For AdaBoost.RT+
#' m4 <- AdaBoost(form, iris, modeltype = "RTPlus", model_pars = NULL); predict(m4, ts)
#'
#' #For BEMBoost
#' m5 <- AdaBoost(form, iris, modeltype = "BEM", model_pars = NULL); predict(m5, ts)
#'
AdaBoost <-
  function(form, data, modeltype, model_pars) {
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
