#' BEMBoost
#'
#' @param form A formula describing the prediction problem
#' @param data A data frame containing the training data
#' @param niter The number of boosting iterations
#' @param BEM Big Error Margin. A value that defines which errors (absolute distance) should be considered as being big.
#'
#' @keywords internal
BEMBoost.train <-
  function(form, data, niter = 100, BEM=0.5) {

  models <- list()
  betas <- c()
  pred.mat <- c()

  ind.y <- which(colnames(data)==as.character(form[[2]]))

  n <- nrow(data)
  weights <- rep(1/n,n) #initialize weights

  err_t <- 0
  for (t in 1:niter) {

    train.ind <- sample(1:n,n,replace=TRUE,prob=weights)
    m <- rpart::rpart(form, data[train.ind,])

    models[[t]] <- m

    f <- predict(m,data)

    ae <- abs(f-data[,ind.y])

    grtBEM <- ae>BEM
    errCnt <- sum(grtBEM)

    if(errCnt==0) break

    upfactor <- n/errCnt
    downfactor <- 1/upfactor

    lwrBEM <- !grtBEM

    weights[grtBEM] <- weights[grtBEM] * upfactor
    weights[lwrBEM] <- weights[lwrBEM] * downfactor

    weights <- weights/sum(weights)
  }

  names(models) <- paste0("M",seq_along(models))

  list(models=models, model_pars=NULL)

}

#' predict method
#'
#' @param model A set of decision trees corresponding to the number of boosting iterations
#' @param newdata A data frame containing the test data
#' @import rpart
#'
#' @keywords internal
BEMBoost.predict <-
  function(models, newdata) {
    preds <- lapply(models, predict, newdata)
    preds <- as.data.frame(preds)

    rowMeans(preds)
  }

#' SMOTEd BEMBoost
#'
#' @param form A formula describing the prediction problem
#' @param data A data frame containing the training data
#' @param niter The number of boosting iterations
#' @param BEM Big Error Margin. A value that defines which errors (absolute distance) should be considered as being big
#' @param perc.O Over-sampling percentage to apply to randomly selected cases with extreme values
#'
#' @keywords internal
SMOTEBoostBEM.train <-
  function(form,
           data,
           niter = 100,
           BEM = 0.5,
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

      ae <- abs(f - data[, ind.y])

      grtBEM <- ae > BEM
      errCnt <- sum(grtBEM)

      if (errCnt == 0)
        break

      upfactor <- n / errCnt
      downfactor <- 1 / upfactor

      lwrBEM <- !grtBEM

      weights[grtBEM] <- weights[grtBEM] * upfactor
      weights[lwrBEM] <- weights[lwrBEM] * downfactor

      weights <- weights / sum(weights)
    }

    names(models) <- paste0("M",seq_along(models))

    list(models=models, model_pars=NULL)
  }
