##' A function to calculate the power under pre-specified effect size, variance, and
##' correlation using Monte Carlo sampling scheme
##' @title A function to calculate the power under pre-specified effect size,
##' variance, and correlation using Monte Carlo sampling scheme by fitting ANCOVA fixed effects model,
##' ANCOVA random effects model, Cox model, and frailty model.
##' @param seed  an integer random seed number.
##' @param n an integer number to specify the number of PDX lines.
##' @param m an integer number to specify the number of individuals per PDX line per treatment.
##' @param beta Treatment effect for the treated group.
##' @param tau2 Error variance of random effect.
##' @param alpha Signifiance level. Default is 0.05.
##' @param lambda Scale parameter of weibull distribution for the baseline hazard.
##' @param nu Shape parameter of weibull distribution for the baseline hazard.
##' @param sigma2 Error variance of log survival time for both treatment groups.
##' @param distr distributional assumption of survival time.
##' @param two.sided Logical value to indicate if a two-sided hypothesis testing is conducted.
##' @param lambdaC Rate parameter of exponential distribution for the hazard of censoring.
##' @param censor logical value of whether a censoring distribution is considered in a data generation setting. Deafult is TRUE.
##' @export
##'

bootfit <- function(seed = 1000, n, m, beta, tau2, alpha = 0.05, lambda = 0.03,
                     nu = 2, sigma2 = 1, distr = c("weibull", "normal"), two.sided = TRUE,
                    lambdaC = 0.1, censor = TRUE) {

  set.seed(100+seed)
  Data <- NULL
  if (distr == "weibull") {
    for (i in 1:n) {
      subY <- vector()
      subX <- vector()
      alphai <- stats::rnorm(1, mean = 0, sd = sqrt(tau2))
      for (j in 1:m) {
        #epsilon <- stats::rnorm(1, mean = 0, sd = sqrt(sigma2))
        f <- exp(alphai)
        U <- runif(1)
        subY[j] <- (-log(U)/lambda/f)^(1/nu)
        subX[j] <- 0
      }
      subDataA <- cbind(i, subY, subX)
      colnames(subDataA) <- c("ID", "Y", "Tx")
      for (j in 1:m) {
        #epsilon <- stats::rnorm(1, mean = 0, sd = sqrt(sigma2))
        f <- exp(beta + alphai)
        U <- runif(1)
        subY[j] <- (-log(U)/lambda/f)^(1/nu)
        subX[j] <- 1
      }
      subDataB <- cbind(i, subY, subX)
      colnames(subDataB) <- c("ID", "Y", "Tx")
      subData <- rbind(subDataA, subDataB)
      Data <- rbind(Data, subData)
    }
  } else if (distr == "normal") {
    for (i in 1:n) {
      subY <- vector()
      subX <- vector()
      alphai <- stats::rnorm(1, mean = 0, sd = sqrt(tau2))
      for (j in 1:m) {
        epsilon <- stats::rnorm(1, mean = 0, sd = sqrt(sigma2))
        subY[j] <- exp(alphai + epsilon)
        subX[j] <- 0
      }
      subDataA <- cbind(i, subY, subX)
      colnames(subDataA) <- c("ID", "Y", "Tx")
      for (j in 1:m) {
        epsilon <- stats::rnorm(1, mean = 0, sd = sqrt(sigma2))
        subY[j] <- exp(beta + alphai + epsilon)
        subX[j] <- 1
      }
      subDataB <- cbind(i, subY, subX)
      colnames(subDataB) <- c("ID", "Y", "Tx")
      subData <- rbind(subDataA, subDataB)
      Data <- rbind(Data, subData)
    }
  } else {
    return(c(FALSE, FALSE))
  }

  Data <- as.data.frame(Data)
  Data <- data.frame(Data, 1)
  colnames(Data)[4] <- "status"

  if (censor) {
    survtime <- vector()
    Data$C <- rexp(nrow(Data), rate = lambdaC)
    for (i in 1:nrow(Data)) {
      if (min(Data$Y[i], Data$C[i]) == Data$Y[i]) {
        survtime[i] <- min(Data$Y[i], Data$C[i])
      } else {
        survtime[i] <- Data$C[i]
        Data$status[i] <- 0
      }
    }
    Data$survtime <- survtime
    # print(sum(Data$status)/nrow(Data))
    CoxRandom <- frailtypack::frailtyPenal(survival::Surv(survtime,status) ~ Tx + cluster(ID),
                                           data=Data, RandDist = "LogN",
                                           print.times = FALSE, maxit = 50, hazard = "Weibull")

    Data <- Data[Data$status == 1, ]
    ANCOVArandom <- try(nlme::lme(fixed = log(survtime) ~ Tx, random = ~1|ID, data = Data),
                        silent = TRUE)


    if ('try-error' %in% class(ANCOVArandom)) {
      Remodel1 <- NA
    } else {
      summodel1 <- summary(ANCOVArandom)
    }

  } else {
    ANCOVArandom <- try(nlme::lme(fixed = log(Y) ~ Tx, random = ~1|ID, data = Data),
                        silent = TRUE)

    if ('try-error' %in% class(ANCOVArandom)) {
      Remodel1 <- NA
    } else {
      summodel1 <- summary(ANCOVArandom)
    }

    CoxRandom <- frailtypack::frailtyPenal(survival::Surv(Y,status) ~ Tx + cluster(ID),
                                           data=Data, RandDist = "LogN",
                                           print.times = FALSE, maxit = 50, hazard = "Weibull")
  }


  if (!'try-error' %in% class(ANCOVArandom)) {
    if (two.sided) {
      p2 <- summodel1$tTable[2, 5]
      if (p2 <= alpha) {
        Remodel1 <- TRUE
      } else {
        Remodel1 <- FALSE
      }
    } else {
      p2 <- pt(coef(summodel1)[2, 4], coef(summodel1)[2, 3], lower = FALSE)
      if (p2 <= alpha) {
        Remodel1 <- TRUE
      } else {
        Remodel1 <- FALSE
      }
    }
  }


  if (CoxRandom$varH == 0) {
    Remodel2 <- NA
  } else {
    if (two.sided) {
      p4 <- CoxRandom$beta_p.value[1]
      if (p4 <= alpha) {
        Remodel2 <- TRUE
      } else {
        Remodel2 <- FALSE
      }
    } else {
      p4 <- pnorm(CoxRandom$coef/sqrt(CoxRandom$varH), lower.tail = TRUE)
      if (p4 <= alpha) {
        Remodel2 <- TRUE
      } else {
        Remodel2 <- FALSE
      }
    }
  }

  HyTest <- c(Remodel1, Remodel2)
  return(HyTest)
}





