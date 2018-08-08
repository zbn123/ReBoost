#' AdaBoost.R2
#'
#' @param form A formula describing the prediction problem
#' @param data A data frame containing the training data
#' @param niter The number of boosting iterations
#' @param power Exponent used to calculate the weight of each decision tree. Common values are 1 (linear), 2 (quadratic, default), 3 (cubic).
#'
#' @keywords internal
AdaBoost.R2.train <- function(form,data,niter=100,power=2) {
  models <- list()
  betas <- c()

  ind.y <- which(colnames(data)==as.character(form[[2]]))

  n <- nrow(data)
  weights <- rep(1/n,n)
  err_t <- 0

  for (i in 1:niter) {
    train.ind <- sample(1:n,n,replace=TRUE,prob=weights)
    m <- rpart::rpart(form,data[train.ind,])

    models[[i]] <- m

    f <- predict(m,data)
    ar <- abs(f-data[,ind.y])
    ar <- (ar/max(ar))^power

    err_t <- sum(weights*ar)

    if(err_t>=0.5) break

    beta_t <- err_t / (1-err_t)
    betas[[i]] <- beta_t

    weights <- weights*(beta_t^(1-err_t))
    weights <- weights/sum(weights)
  }
  names(models) <- paste0("M",seq_along(models))

  model_pars = list(beta=betas)

  list(models=models,model_pars=model_pars)
}

#' AdaBoost.R2 predict method
#'
#' @param models A set of decision trees corresponding to the number of boosting iterations
#' @param newdata A data frame containing the test data
#' @import rpart
#'
#' @keywords internal
AdaBoost.R2.predict <-
  function(models, betas, newdata) {
    preds <- lapply(models, predict, newdata)
    preds <- as.data.frame(preds)

    apply(preds, 1,
          function(o) {
            stats::weighted.mean(o, betas)
          })
  }

#' SMOTEd AdaBoost.R2
#'
#' @param form A formula describing the prediction problem
#' @param data A data frame containing the training data
#' @param niter The number of boosting iterations
#' @param power Exponent used to calculate the weight of each decision tree. Common values are 1 (linear), 2 (quadratic, default), 3 (cubic).
#' @param perc.O Over-sampling percentage to apply to randomly selected cases with extreme values
#'
#' @keywords internal
SMOTEBoost.R2.train <-
  function(form,
           data,
           niter = 100,
           power = 2,
           perc.O = 1.5) {

    models <- list()
    betas <- c()

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

      ar <- abs(f - data[, ind.y])
      ar <- (ar / max(ar)) ^ power

      err_t <- sum(weights * ar)

      if (err_t >= 0.5)
        break

      beta_t <- err_t / (1 - err_t)
      betas[[t]] <- beta_t

      weights <- weights * (beta_t ^ (1 - err_t))
      weights <- weights / sum(weights)
    }

    names(models) <- paste0("M",seq_along(models))

    model_pars = list(beta=betas)

    list(models=models,model_pars=model_pars)
  }
