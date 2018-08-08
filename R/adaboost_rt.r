#' AdaBoost.RT
#'
#' @param form A formula describing the prediction problem
#' @param data A data frame containing the training data
#' @param niter The number of boosting iterations
#' @param thr Absolute Relative Error threshold used to define prediction cases as hard to predict
#' @param power Exponent used to calculate the weight of each decision tree. Common values are 1 (linear), 2 (quadratic, default), 3 (cubic).
#'
#' @keywords internal
AdaBoost.RT.train <-
  function(form,
           data,
           niter = 100,
           thr = 0.1,
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
      m <- rpart::rpart(form, data[train.ind, ])

      models[[t]] <- m
      f <- predict(m, data)

      are <- abs((f - data[, ind.y]) / data[, ind.y])

      err_t <- sum(weights[are > thr])

      beta_t <- err_t ^ power
      betas[[t]] <- beta_t

      weights[are <= thr] <- beta_t
      weights[are > thr] <- 1
      weights <- weights / sum(weights)
    }

    names(models) <- paste0("M",seq_along(models))

    model_pars <- list(beta=betas)

    list(models=models,model_pars=model_pars)
  }

#' AdaBoost.RT predict method
#'
#' @param models A set of decision trees corresponding to the number of boosting iterations
#' @param newdata A data frame containing the test data
#' @param betas Model weights
#' @import rpart
#'
#' @keywords internal
AdaBoost.RT.predict <-
  function(models, newdata, betas) {
    num <- 0
    pred.mat <- c()
    for (t in 1:length(models)) {
      preds <- predict(models[[t]], newdata)
      pred.mat <- cbind(pred.mat, preds)
      num <- num + (log(1 / betas[t]) * preds)
    }

    num / sum(log(1 / betas))
  }

#' SMOTEd AdaBoost.RT
#'
#' @param form A formula describing the prediction problem
#' @param data A data frame containing the training data
#' @param niter The number of boosting iterations
#' @param thr Absolute Relative Error threshold used to define prediction cases as hard to predict
#' @param power Exponent used to calculate the weight of each decision tree. Common values are 1 (linear), 2 (quadratic, default), 3 (cubic).
#' @param perc.O Over-sampling percentage to apply to randomly selected cases with extreme values
#'
#' @keywords internal
SMOTEBoost.RT.train <-
  function(form,
           data,
           niter = 100,
           thr = 0.1,
           power = 2,
           perc.O = 1.5) {
    models <- list()
    betas <- c()
    pred.mat <- c()

    ind.y <- which(colnames(data) == as.character(form[[2]]))

    n <- nrow(data)
    weights <- rep(1 / n, n)
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

      are <- abs(as.numeric((f - data[, ind.y]) / data[, ind.y]))
      err_t <- sum(weights[are > thr])

      beta_t <- err_t ^ power
      betas[[t]] <- beta_t

      weights[are <= thr] <- beta_t
      weights[are > thr] <- 1
      weights <- weights / sum(weights)
    }

    names(models) <- paste0("M",seq_along(models))

    model_pars <- list(beta=betas)

    list(models=models,model_pars=model_pars)
  }
