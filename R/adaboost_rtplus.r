#' Create an AdaBoost.RT+ model
#'
#' @param form A formula describing the prediction problem
#' @param data A data frame containing the training data
#' @param niter The number of boosting iterations
#' @param thr Absolute Relative Error threshold used to define prediction cases as hard to predict
#' @param power Exponent used to calculate the weight of each decision tree. Common values are 1 (linear), 2 (quadratic, default), 3 (cubic).
#' @param sigma Value used for regularization (default 0.5). If no regularization is expected, use NULL
#'
#' @keywords internal
AdaBoost.RTPlus.train <-
  function(form,
           data,
           niter = 100,
           thr = 0.01,
           power = 2,
           sigma = 0.5) {

    models <- list()
    betas <- c()
    pred.mat <- c()
    train_pred.mat <- c()

    ind.y <- which(colnames(data) == as.character(form[[2]]))

    n <- nrow(data)
    weights <- rep(1 / n, n) #initialize weights
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

    for (t in 1:niter) {
      preds <- predict(models[[t]], data)
      train_pred.mat <- cbind(train_pred.mat, preds)
    }

    delta <- 0

    if (is.null(sigma)) {
      # no regularization
      delta <- t(MASS::ginv(t(train_pred.mat))) %*% data[, ind.y]
    } else {
      delta <-
        t(MASS::ginv(train_pred.mat %*% t(train_pred.mat) +
                 sigma * diag(nrow(data))) %*% train_pred.mat) %*% data[, ind.y]
    }

    names(models) <- paste0("M",seq_along(models))

    model_pars <- list(delta=delta)

    list(models = models, model_pars=model_pars)
  }

#' AdaBoost.RTPlus predict method
#'
#' @param models A set of decision trees corresponding to the number of boosting iterations
#' @param newdata A data frame containing the test data
#' @param delta Model weights
#' @import rpart
#'
#' @keywords internal
AdaBoost.RTPlus.predict <-
  function(models, newdata, delta) {
    pred.mat<-c()
    for (t in 1:length(models)) {
      preds <- predict(models[[t]], newdata)
      pred.mat <- cbind(pred.mat, preds)
    }
    (pred.mat %*% delta)[,1]
  }

#' Create a SMOTEd AdaBoost.RT+ model
#'
#' @param form A formula describing the prediction problem
#' @param data A data frame containing the training data
#' @param niter The number of boosting iterations
#' @param thr Absolute Relative Error threshold used to define prediction cases as hard to predict
#' @param power Exponent used to calculate the weight of each decision tree. Common values are 1 (linear), 2 (quadratic, default), 3 (cubic).
#' @param sigma Value used for regularization (default 0.5). If no regularization is expected, use NULL
#' @param perc.O Over-sampling percentage to apply to randomly selected cases with extreme values
#'
#' @keywords internal
SMOTEBoost.RTPlus.train <-
  function(form,
           data,
           niter = 100,
           thr = 0.01,
           power = 2,
           sigma = 0.5,
           perc.O = 1.5) {

    models <- list()
    betas <- c()
    pred.mat <- c()
    train_pred.mat <- c()

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
      are <- abs((f - data[, ind.y]) / data[, ind.y])

      err_t <- sum(weights[are > thr])

      beta_t <- err_t ^ power
      betas[[t]] <- beta_t

      weights[are <= thr] <- beta_t
      weights[are > thr] <- 1
      weights <- weights / sum(weights)
    }

    for (t in 1:niter) {
      preds <- predict(models[[t]], data)
      train_pred.mat <- cbind(train_pred.mat, preds)
    }

    delta <- 0

    if (is.null(sigma)) {
      # no regularization
      delta <- t(MASS::ginv(t(train_pred.mat))) %*% data[, ind.y]
    } else {
      delta <-
        t(MASS::ginv(train_pred.mat %*% t(train_pred.mat) +
                 sigma * diag(nrow(data))) %*% train_pred.mat) %*% data[, ind.y]
    }

    names(models) <- paste0("M",seq_along(models))

    model_pars <- list(delta=delta)

    list(models = models, model_pars=model_pars)
  }
