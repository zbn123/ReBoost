setClassUnion("optlist", c("list","NULL"))

#' SmoteBoost-class
#'
#' \strong{SmoteBoost} is a S4 class that contains
#'
#' @slot model A set of decision trees corresponding to the number of boosting iterations
#'
#' @slot form A formula describing the prediction problem
#'
#' @slot model_pars Parameters used to create decision trees and guide the boosting process
#'
#' @slot modeltype Type of boosting method
#'
#' @slot perc_over Over-sampling percentage to apply to randomly selected cases with extreme values.
#'
#' This package enables the implementation of 5 different proposals for SMOTEBoost described in the work of Moniz et al. (2018), including those based on AdaBoost.R2 (Drucker1997), AdaBoost.RQ, AdaBoost.RT (Solomatine2004), AdaBoost.RT+ (Kankanala2014) and BEMBoost (Feely2000).
#'
#' @references
#' N. Moniz, R. P. Ribeiro, V. Cerqueira, N. Chawla (2018). "SMOTEBoost for Regression: Improving the Prediction of Extreme Values", in Proceedings of the 5th IEEE International Conference on Data Science and Advanced Analytics, IEEE Xplore. 2018.
#' H. Drucker, “Improving regressors using boosting techniques”, in Proceedings of the Fourteenth International Conference on Machine Learning, ICML’97, Morgan Kaufmann Publishers Inc., pp. 107–115. 1997.
#' R. Feely, “Predicting stock market volatility using neural networks", Ph.D. Dissertation, Trinity College, Dublin, 2000.
#' D. P. Solomatine and D. L. Shrestha, “Adaboost.rt: a boosting algorithm for regression problems,” in 2004 IEEE International Joint Conference on Neural Networks, vol. 2, pp. 1163–1168 vol.2, 2004.
#' P. Kankanala, S. Das, and A. Pahwa, “Adaboost+: An ensemble learning approach for estimating weather-related outages in distribution systems", IEEE Transactions on Power Systems, vol. 29, no. 1, pp. 359–367, 2014.
#'
#' @export
setClass("SmoteBoost",
         slots = c(model = "list",
                   form = "formula",
                   model_pars = "optlist",
                   modeltype = "character",
                   perc_over = "numeric")
)

#' SmoteBoost
#'
#' \strong{SmoteBoost} is a S4 class that contains
#'
#' @slot model A set of decision trees corresponding to the number of boosting iterations
#'
#' @slot form A formula describing the prediction problem
#'
#' @slot model_pars Parameters used to create decision trees and guide the boosting process
#'
#' @slot modeltype Type of boosting method
#'
#' @slot perc_over Over-sampling percentage to apply to randomly selected cases with extreme values.
#'
#' This package enables the implementation of 5 different proposals for SMOTEBoost described in the work of Moniz et al. (2018), including those based on AdaBoost.R2 (Drucker1997), AdaBoost.RQ, AdaBoost.RT (Solomatine2004), AdaBoost.RT+ (Kankanala2014) and BEMBoost (Feely2000).
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
#' #For SMOTEBoost.RQ
#' m1 <- SmoteBoost(form, tr, modeltype = "RQ", model_pars = NULL); predict(m1, ts)
#'
#' #For SMOTEBoost.R2
#' m2<-SmoteBoost(form, iris, modeltype = "R2", model_pars = NULL); predict(m2, ts)
#'
#' #For SMOTEBoost.RT
#' m3<-SmoteBoost(form, iris, modeltype = "RT", model_pars = NULL); predict(m3, ts)
#'
#' #For SMOTEBoost.RT+
#' m4<-SmoteBoost(form, iris, modeltype = "RTPlus", model_pars = NULL); predict(m4, ts)
#'
#' #For SMOTEBoost.BEM
#' m5<-SmoteBoost(form, iris, modeltype = "BEM", model_pars = NULL); predict(m5, ts)
#'
SmoteBoost <-
  function(form, data, modeltype, model_pars, perc_over=.8) {
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
                              niter = mpars$niter,
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
      "SmoteBoost",
      model = model$models,
      form = form,
      model_pars = model$model_pars,
      modeltype = modeltype,
      perc_over = perc_over)
  }


setMethod("show",
          signature("SmoteBoost"),
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
          model_pars <- c(model_pars, niter = niter)
        }

        if (is.null(model_pars[["thr"]])) {
          thr <- .01
          model_pars <- c(model_pars, thr = thr)
        }

        if (is.null(model_pars[["power"]])) {
          power <- 2
          model_pars <- c(model_pars, power = power)
        }

        if (is.null(model_pars[["sigma"]])) {
          sigma <- .5
          model_pars <- c(model_pars, sigma = sigma)
        }
      },
      "RQ" = {
        if (is.null(model_pars)) {
          return(list(niter = 100, power = 2))
        }

        if (is.null(model_pars[["niter"]])) {
          niter <- 100
          model_pars <- c(model_pars, niter = niter)
        }

        if (is.null(model_pars[["power"]])) {
          power <- 2
          model_pars <- c(model_pars, power = power)
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
          model_pars <- c(model_pars, niter = niter)
        }

        if (is.null(model_pars[["power"]])) {
          power <- 2
          model_pars <- c(model_pars, power = power)
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
          model_pars <- c(model_pars, niter = niter)
        }

        if (is.null(model_pars[["thr"]])) {
          thr <- .01
          model_pars <- c(model_pars, thr = thr)
        }

        if (is.null(model_pars[["power"]])) {
          power <- 2
          model_pars <- c(model_pars, power = power)
        }

        if (is.null(model_pars[["sigma"]])) {
          sigma <- .5
          model_pars <- c(model_pars, sigma = sigma)
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
          model_pars <- c(model_pars, niter = niter)
        }

        if (is.null(model_pars[["BEM"]])) {
          BEM <- 0.5
          model_pars <- c(model_pars, BEM = BEM)
        }

      }
    )
  }


