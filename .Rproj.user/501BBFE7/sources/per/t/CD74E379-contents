#' adaSMOTE
#'
#' @param form A formula describing the prediction problem
#' @param dat A data frame containing the training data
#' @param perc.o Over-sampling percentage to apply to randomly selected cases with extreme values
#' @param rel.thr Relevance threshold for considering case as having extreme value
#' @param k Number of neighbours to use in SMOTE interpolation
#' @param pc Relevance function
#'
#' @keywords internal
adaSMOTE <-
  function(form, dat, perc.o, rel.thr = 0.9, k, pc = NULL) {

    y <- dat[, as.character(form[[2]])]

    if (length(pc) != 3) {
      pc <- UBL::phi.control(y = y,
                             method = "extremes",
                             coef = 1.5)
    }

    new.dat <- c()

    if (any(pc$control.pts[c(2, 8)] == 1)) {
      percs <- list()

      if (pc$control.pts[2] == 1) {
        if (length(grDevices::boxplot.stats(y)$out <= pc$control.pts[1]) > 1) {
          percs <- c(percs, perc.o)
        } else {
          percs <- c(percs, 1)
        }
      }

      percs <- c(percs, 1)

      if (pc$control.pts[8] == 1) {
        if (length(grDevices::boxplot.stats(y)$out >= pc$control.pts[7]) > 1) {
          percs <- c(percs, perc.o)
        } else {
          percs <- c(percs, 1)
        }
      }

      if (any(sapply(dat, is.numeric) == FALSE)) {
        #If there's any nominal predictor, use HEOM distance
        new.dat <-
          UBL::SmoteRegress(
            form,
            dat,
            rel = pc,
            thr.rel = rel.thr,
            C.perc = percs,
            k = k,
            dist = "HEOM"
          )
      } else {
        #If all predictors are numerical, use Euclidean distance
        new.dat <-
          UBL::SmoteRegress(
            form,
            dat,
            rel = pc,
            thr.rel = rel.thr,
            C.perc = percs,
            k = k,
            dist = "Euclidean"
          )
      }

    } else {
      warning("Did not found any extreme cases. Returning the original train set.")
      new.dat <- dat
    }
    new.dat
  }
