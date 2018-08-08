#' Predict method for objects of SmoteBoost class
#'
#' @param object A SmoteBoost object
#' @param newdata A data frame containing the test data
#'
#' @export
setMethod("predict",
          signature("SmoteBoost"),
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



#' Predict method for objects of AdaBoost class
#'
#' @param object An AdaBoost object
#' @param newdata A data frame containing the test data
#'
#' @export
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
