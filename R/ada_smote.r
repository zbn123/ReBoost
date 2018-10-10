#' adaSMOTE
#'
#' @description Applies the SMOTEr resampling strategy to a given data set.
#'
#' @param form A formula describing the prediction problem
#' @param dat A data frame containing the training data
#' @param perc.o Over-sampling percentage to apply to randomly selected cases with extreme values - defaults to 1.5
#' @param rel.thr Relevance threshold for considering case as having extreme value
#' @param k Number of neighbours to use in SMOTE interpolation - defaults to 3
#' @param pc Relevance function
#'
#' @keywords internal
#'
#' @references L. Torgo, P. Branco, R. P. Ribeiro, et al. “Re-sampling Strategies for Regression”. In: Expert Systems 32.3 (2015), pp. 465-476.
#'
#'
#' P. Branco, R. Ribeiro and L. Torgo. “A UBL: an R package for Utility-based Learning”. In: CoRR abs/1604.08079 (2016).
#'
adaSMOTE <-
  function(form, dat, perc.o=1.5, rel.thr = 0.9, k=3, pc = NULL) {

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
