bootfit <- function(seed = 1000, n, m, beta0 = 5, beta, tau2, alpha = 0.05, lambda = 0.03,
                     nu = 2, sigma2 = 1, distr = c("Weibull", "normal"), two.sided = TRUE,
                    Ct = 5, censor = TRUE, model = c("both", "ANOVA", "Cox-frailty"),
                    fixed.effect = FALSE) {

  set.seed(seed)
  Data <- NULL
  if (distr == "Weibull") {
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
        subY[j] <- exp(beta0 + alphai + epsilon)
        subX[j] <- 0
      }
      subDataA <- cbind(i, subY, subX)
      colnames(subDataA) <- c("ID", "Y", "Tx")
      for (j in 1:m) {
        epsilon <- stats::rnorm(1, mean = 0, sd = sqrt(sigma2))
        subY[j] <- exp(beta0 + beta + alphai + epsilon)
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

  Remodel1 <- Remodel2 <- NA

  Data <- as.data.frame(Data)
  Data <- data.frame(Data, 1)
  colnames(Data)[4] <- "status"

  if (censor) {
    survtime <- vector()
    Data$C <- Ct
    for (i in 1:nrow(Data)) {
      if (min(Data$Y[i], Data$C[i]) == Data$Y[i]) {
        survtime[i] <- min(Data$Y[i], Data$C[i])
      } else {
        survtime[i] <- Data$C[i]
        Data$status[i] <- 0
      }
    }
    Data$survtime <- survtime

    Data <- Data[, c(1, 6, 3, 4)]
    colnames(Data)[2] <- "Y"
    censor.rate <- 1 - sum(Data$status)/nrow(Data)

  } else {
    censor.rate <- 0
  }

  if (model == "both") {

    if (fixed.effect) {

      ANOCOVAfixed <- try(lm(log(Y) ~ Tx + factor(ID), data = Data), silent = TRUE)
      if ('try-error' %in% class(ANOCOVAfixed)) {
        Remodel1.fixed <- NA
      } else {
        summodel1.fixed <- summary(ANOCOVAfixed)
      }

      Coxfix <- try(frailtypack::frailtyPenal(survival::Surv(Y,status) ~ Tx + factor(ID),
                                              data=Data, print.times = FALSE, maxit = 50,
                                              hazard = "Weibull"),
                    silent = TRUE)

      if ('try-error' %in% class(Coxfix)) {
        Remodel2.fixed <- NA
      }

    } else {
      CoxRandom <- try(frailtypack::frailtyPenal(survival::Surv(Y,status) ~ Tx + cluster(ID),
                                                 data=Data, RandDist = "LogN",
                                                 print.times = FALSE, maxit = 50, hazard = "Weibull"),
                       silent = TRUE)

      if ('try-error' %in% class(CoxRandom)) {
        Remodel2 <- NA
      }

      Data <- Data[Data$status == 1, ]
      ANCOVArandom <- try(nlme::lme(fixed = log(Y) ~ Tx, random = ~1|ID, data = Data),
                          silent = TRUE)


      if ('try-error' %in% class(ANCOVArandom)) {
        Remodel1 <- NA
      } else {
        summodel1 <- summary(ANCOVArandom)
      }
    }


  } else if (model == "ANOVA") {

    Data <- Data[Data$status == 1, ]

    if (fixed.effect) {
      ANOCOVAfixed <- try(lm(log(Y) ~ Tx + factor(ID), data = Data), silent = TRUE)
      if ('try-error' %in% class(ANOCOVAfixed)) {
        Remodel1.fixed <- NA
      } else {
        summodel1.fixed <- summary(ANOCOVAfixed)
      }
    } else {
      ANCOVArandom <- try(nlme::lme(fixed = log(Y) ~ Tx, random = ~1|ID, data = Data),
                          silent = TRUE)

      if ('try-error' %in% class(ANCOVArandom)) {
        Remodel1 <- NA
      } else {
        summodel1 <- summary(ANCOVArandom)
      }
    }

  } else if (model == "Cox-frailty") {

    if (fixed.effect) {
      Coxfix <- try(frailtypack::frailtyPenal(survival::Surv(Y,status) ~ Tx + factor(ID),
                                              data=Data, print.times = FALSE, maxit = 50,
                                              hazard = "Weibull"),
                    silent = TRUE)

      if ('try-error' %in% class(Coxfix)) {
        Remodel2.fixed <- NA
      }
    } else {
      CoxRandom <- try(frailtypack::frailtyPenal(survival::Surv(Y,status) ~ Tx + cluster(ID),
                                                 data=Data, RandDist = "LogN",
                                                 print.times = FALSE, maxit = 50, hazard = "Weibull"),
                       silent = TRUE)

      if ('try-error' %in% class(CoxRandom)) {
        Remodel2 <- NA
      }
    }

  } else {
    stop("Please choose the one of the following options for modeling: Weibull, normal, and both.")
  }

  if (model == "Cox-frailty") {

    if (fixed.effect) {
      if ('try-error' %in% class(Coxfix) | determinant(Coxfix$varH)$modulus == 0) {
        Remodel2.fixed <- NA
      } else {
        if (two.sided) {
          p4 <- Coxfix$beta_p.value[1]
          if (p4 <= alpha) {
            Remodel2.fixed <- TRUE
          } else {
            Remodel2.fixed <- FALSE
          }
        } else {
          p4 <- pnorm(Coxfix$coef[1]/sqrt(Coxfix$varH[1, 1]), lower.tail = TRUE)
          if (p4 <= alpha) {
            Remodel2.fixed <- TRUE
          } else {
            Remodel2.fixed <- FALSE
          }
        }
      }
    } else {
      if ('try-error' %in% class(CoxRandom) | CoxRandom$varH == 0) {
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
    }

    if (fixed.effect) {
      result <- list(censor.rate = censor.rate, Remodel2.fixed = Remodel2.fixed)
    } else {
      result <- list(censor.rate = censor.rate, Remodel2 = Remodel2)
    }

    return(result)

  } else if (model == "ANOVA") {

    if (fixed.effect) {
      if (!'try-error' %in% class(ANOCOVAfixed)) {
        if (two.sided) {
          p2 <- summodel1.fixed$coefficients[2, 4]
          if (!is.nan(p2)) {
            if (p2 <= alpha) {
              Remodel1.fixed <- TRUE
            } else {
              Remodel1.fixed <- FALSE
            }
          } else {
            Remodel1.fixed <- NA
          }
        } else {
          p2 <- pt(summodel1.fixed$coefficients[2, 3], ANOCOVAfixed$df.residual, lower.tail = FALSE)
          if (p2 <= alpha) {
            Remodel1.fixed <- TRUE
          } else {
            Remodel1.fixed <- FALSE
          }
        }
      }
    } else {
      if (!'try-error' %in% class(ANCOVArandom)) {
        if (two.sided) {
          p2 <- summodel1$tTable[2, 5]
          if (!is.nan(p2)) {
            if (p2 <= alpha) {
              Remodel1 <- TRUE
            } else {
              Remodel1 <- FALSE
            }
          } else {
            Remodel1 <- NA
          }
        } else {
          p2 <- pt(coef(summodel1)[2, 4], coef(summodel1)[2, 3], lower.tail = FALSE)
          if (p2 <= alpha) {
            Remodel1 <- TRUE
          } else {
            Remodel1 <- FALSE
          }
        }
      }
    }

    if (fixed.effect) {
      result <- list(censor.rate = censor.rate, Remodel1.fixed = Remodel1.fixed)
    } else {
      result <- list(censor.rate = censor.rate, Remodel1 = Remodel1)
    }

    return(result)

  } else {

    if (fixed.effect) {
      if (!'try-error' %in% class(ANOCOVAfixed)) {
        if (two.sided) {
          p2 <- summodel1.fixed$coefficients[2, 4]
          if (!is.nan(p2)) {
            if (p2 <= alpha) {
              Remodel1.fixed <- TRUE
            } else {
              Remodel1.fixed <- FALSE
            }
          } else {
            Remodel1.fixed <- NA
          }
        } else {
          p2 <- pt(summodel1.fixed$coefficients[2, 3], ANOCOVAfixed$df.residual, lower.tail = FALSE)
          if (p2 <= alpha) {
            Remodel1.fixed <- TRUE
          } else {
            Remodel1.fixed <- FALSE
          }
        }
      }

      if ('try-error' %in% class(Coxfix) | determinant(Coxfix$varH)$modulus == 0) {
        Remodel2.fixed <- NA
      } else {
        if (two.sided) {
          p4 <- Coxfix$beta_p.value[1]
          if (p4 <= alpha) {
            Remodel2.fixed <- TRUE
          } else {
            Remodel2.fixed <- FALSE
          }
        } else {
          p4 <- pnorm(Coxfix$coef[1]/sqrt(Coxfix$varH[1, 1]), lower.tail = TRUE)
          if (p4 <= alpha) {
            Remodel2.fixed <- TRUE
          } else {
            Remodel2.fixed <- FALSE
          }
        }
      }


    } else {
      if (!'try-error' %in% class(ANCOVArandom)) {
        if (two.sided) {
          p2 <- summodel1$tTable[2, 5]
          if (!is.nan(p2)) {
            if (p2 <= alpha) {
              Remodel1 <- TRUE
            } else {
              Remodel1 <- FALSE
            }
          } else {
            Remodel1 <- NA
          }

        } else {
          p2 <- pt(coef(summodel1)[2, 4], coef(summodel1)[2, 3], lower.tail = FALSE)
          if (p2 <= alpha) {
            Remodel1 <- TRUE
          } else {
            Remodel1 <- FALSE
          }
        }
      }

      if ('try-error' %in% class(CoxRandom) | CoxRandom$varH == 0) {
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
    }

    if (fixed.effect) {
      result <- list(censor.rate = censor.rate, Remodel1.fixed = Remodel1.fixed, Remodel2.fixed = Remodel2.fixed)
    } else {
      result <- list(censor.rate = censor.rate, Remodel1 = Remodel1, Remodel2 = Remodel2)
    }

    return(result)

  }


}





