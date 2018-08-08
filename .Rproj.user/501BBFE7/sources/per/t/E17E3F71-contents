#' AdaBoost.RQ
#'
#' @param form A formula describing the prediction problem
#' @param data A data frame containing the training data
#' @param niter The number of boosting iterations
#' @param power Exponent used to calculate the weight of each decision tree. Common values are 1 (linear), 2 (quadratic, default), 3 (cubic).
#'
#' @keywords internal
AdaBoost.RQ.train <-
  function(form,
           data,
           niter = 100,
           power = 2) {
    models <- list()
    betas <- c()
    pred.mat <- c()

    ind.y <- which(colnames(data) == as.character(form[[2]]))

    n <- nrow(data)
    weights <- rep(1 / n, n)
    err_t <- 0

    for (t in 1:niter) {
      train.ind <- sample(1:n, n, replace = TRUE, prob = weights)
      m <- rpart::rpart(form, data[train.ind,])
      models[[t]] <- m

      f <- predict(m, data)

      abs.err <- abs(as.numeric((f - data[, ind.y])))

      large.err.ind <-
        which(abs.err > grDevices::boxplot.stats(abs.err)$stats[3])

      err_t <- sum(weights[large.err.ind])

      beta_t <- err_t ^ power
      betas[[t]] <- beta_t

      weights[] <- beta_t
      weights[large.err.ind] <- 1

      weights <- weights / sum(weights)
    }
    names(models) <- paste0("M",seq_along(models))

    model_pars <- list(beta=betas)

    list(models=models,model_pars=model_pars)
  }

#' AdaBoost.RQ predict method
#'
#' @param models A set of decision trees corresponding to the number of boosting iterations
#' @param newdata A data frame containing the test data
#' @param betas Model weights
#' @import rpart
#'
#' @keywords internal
AdaBoost.RQ.predict <-
  function(models, newdata, betas) {
    pred.mat<-c()
    num <- 0
    for (i in 1:length(models)) {
      preds <- predict(models[[i]], newdata)
      pred.mat <- cbind(pred.mat, preds)
      num <- num + (log(1 / betas[i]) * preds)
    }
    finalpreds <- num / sum(log(1 / betas))
    finalpreds
  }

#' SMOTEd AdaBoost.RQ
#'
#' @param form A formula describing the prediction problem
#' @param data A data frame containing the training data
#' @param niter The number of boosting iterations
#' @param power Exponent used to calculate the weight of each decision tree. Common values are 1 (linear), 2 (quadratic, default), 3 (cubic).
#' @param perc.O po
#'
#' @keywords internal
SMOTEBoost.RQ.train <-
  function(form,
           data,
           niter = 100,
           power = 2,
           perc.O = 1.5) {
    models <- list()
    betas <- c()
    pred.mat <- c()

    ind.y <- which(colnames(data) == as.character(form[[2]]))

    n <- nrow(data) #size of train

    weights <- rep(1 / n, n) #initialize weights

    err_t <- 0

    pc <-
      UBL::phi.control(y = data[, ind.y],
                       method = "extremes",
                       coef = 1.5)

    for (t in 1:niter) {
      train.ind <- sample(1:n, n, replace = TRUE, prob = weights)
      new.train <- data[train.ind, ]
      new.train <- adaSMOTE(form, new.train, perc.O, 0.9, k = 3, pc)

      m <- rpart::rpart(form, new.train)

      models[[t]] <- m

      f <- predict(m, data)

      abs.err <- abs(as.numeric((f - data[, ind.y])))

      large.err.ind <- which(abs.err > grDevices::boxplot.stats(abs.err)$stats[3])

      err_t <- sum(weights[large.err.ind])
      beta_t <- err_t ^ power
      betas[[t]] <- beta_t

      weights[] <- beta_t
      weights[large.err.ind] <- 1

      weights <- weights / sum(weights)

    }
    names(models) <- paste0("M",seq_along(models))

    model_pars <- list(beta=betas)

    list(models=models,model_pars=model_pars)
}
