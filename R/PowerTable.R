PowerTable <- function(n, m, beta0 = 5, beta, tau2 = 0.5, alpha = 0.05, lambda = 0.03,
                       nu = 2, sigma2 = 1, two.sided = TRUE, distr = c("Weibull", "normal"),
                       Ct = 5, censor = TRUE, sim = 1000,
                       print = c("both", "ANOVA", "Cox-frailty"), ncores = NULL) {

  if (is.null(ncores)) {
    ncores <- parallel::detectCores()
  }

  if (!print %in% c("both", "ANOVA", "Cox-frailty")) {
    stop("Please choose the one of the following options for modeling: Weibull, normal, and both.")
  }

  SumPowertable <- NULL
  for (j in 1:length(n)) {
    Powertable <- matrix(NA, nrow = length(m), ncol = 7)
    for (i in 1:length(m)) {
      Model1 <- vector()
      Model2 <- vector()
      censor.rate <- vector()
      if (distr == "Weibull") {
        a <- simfit(sim = sim, n = n[j], m = m[i], beta0 = beta0, beta = beta, tau2 = tau2,
                    alpha = alpha, lambda = lambda, nu = nu, distr = distr, two.sided = two.sided,
                    Ct = Ct, censor = censor, model = print, ncores = ncores)
      } else if (distr == "normal") {
        a <- simfit(sim = sim, n = n[j], m = m[i], beta0 = beta0, beta = beta, tau2 = tau2,
                    alpha = alpha, sigma2 = sigma2, distr = distr, two.sided = two.sided,
                    Ct = Ct, censor = censor, model = print, ncores = ncores)
      } else {
        return(0)
      }

      Powertable[i, 2] <- m[i]

      if (print == "both") {

        for (k in 1:sim) {
          Model1[k] <- a[[k]]$Remodel1
          Model2[k] <- a[[k]]$Remodel2
          censor.rate[k] <- a[[k]]$censor.rate
        }
        Model1 <- as.logical(Model1)
        Model2 <- as.logical(Model2)
        Powertable[i, 3] <- sum(Model1, na.rm = TRUE)/sum(!is.na(Model1))
        Powertable[i, 4] <- sum(Model2, na.rm = TRUE)/sum(!is.na(Model2))

      } else if (print == "ANOVA") {

        for (k in 1:sim) {
          Model1[k] <- a[[k]]$Remodel1
          censor.rate[k] <- a[[k]]$censor.rate
        }
        Model1 <- as.logical(Model1)
        Powertable[i, 3] <- sum(Model1, na.rm = TRUE)/sum(!is.na(Model1))

      } else {

        for (k in 1:sim) {
          Model2[k] <- a[[k]]$Remodel2
          censor.rate[k] <- a[[k]]$censor.rate
        }

        Model2 <- as.logical(Model2)
        Powertable[i, 4] <- sum(Model2, na.rm = TRUE)/sum(!is.na(Model2))

      }
      Powertable[i, 7] <- mean(censor.rate)*100

    }
    Powertable[, 1] <- n[j]
    Powertable <- as.data.frame(Powertable)
    colnames(Powertable) <- c("NofLine", "NofAnimals", "ANOVArandom", "Coxrandom", "censoringrate")
    SumPowertable <- rbind(SumPowertable, Powertable)
  }

  class(SumPowertable) <- "PowerTable"

  SumPowertable$beta0 <- beta0
  SumPowertable$beta <- beta
  SumPowertable$lambda <- lambda
  SumPowertable$nu <- nu
  SumPowertable$tau2 <- tau2
  SumPowertable$Ct <- Ct
  SumPowertable$nsim <- sim
  SumPowertable$sigma2 <- sigma2
  SumPowertable$censor <- censor
  SumPowertable$print <- print
  SumPowertable$call <- match.call()

  return(SumPowertable)

}
