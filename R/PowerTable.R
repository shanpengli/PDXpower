##' @title A function to obtain a power table with the combination of
##' various number of PDX lines and number of individuals per PDX lines per treatment.
##' @param n an integer number to specify the number of PDX lines.
##' @param m an integer number to specify the number of individuals per PDX line per treatment.
##' @param beta Treatment effect for the treated group.
##' @param sigma2 Error variance of log survival time for both treatment groups.
##' @param alpha Signifiance level. Default is 0.05.
##' @param lambda Scale parameter of weibull distribution for the baseline hazard.
##' @param nu Shape parameter of weibull distribution for the baseline hazard.
##' @param two.sided A logical value to indicate if a two-sided hypothesis testing is conducted.
##' Default is TRUE.
##' @param sim Number of Monte Carlo samples to be generated. Default is 1000.
##' @param distr Distributional assumption of the simulated event time.
##' @param lambdaC Rate parameter of exponential distribution for the hazard of censoring.
##' @param censor logical value of whether a censoring distribution is considered in a data generation setting. Deafult is TRUE.
##' @param ncores number of cores for parallel computation.
##' @examples
##'
##' require(PDXpower)
##' n = c(4, 6)
##' m = c(1, 2)
##' a <- PowerTable(n = n, m = m, betaB = 0,
##'                 tau2 = 0.5, sim = 10)
##' plotpower(a)
##' @seealso \code{\link{plotpower}}
##' @export
##'

PowerTable <- function(n, m, beta, tau2 = 0.5, alpha = 0.05, lambda = 0.03,
                       nu = 2, sigma2 = 1, two.sided = TRUE, distr = c("weibull", "normal"),
                       lambdaC = 0.1, censor = TRUE, sim = 1000, ncores = 10) {

  if (ncores > parallel::detectCores()) {
    ncores <- parallel::detectCores()
  }

  SumPowertable <- NULL
  for (j in 1:length(n)) {
    Powertable <- matrix(0, nrow = length(m), ncol = 4)
    for (i in 1:length(m)) {
      Model1 <- vector()
      Model2 <- vector()
      if (distr == "weibull") {
        a <- simfit(sim = sim, n = n[j], m = m[i], beta = beta, tau2 = tau2,
                    alpha = alpha, lambda = lambda, nu = nu, distr = distr, two.sided = two.sided,
                    lambdaC = lambdaC, censor = censor, ncores = ncores)
      } else if (distr == "normal") {
        a <- simfit(sim = sim, n = n[j], m = m[i], beta = beta, tau2 = tau2,
                    alpha = alpha, sigma2 = sigma2, distr = distr, two.sided = two.sided,
                    lambdaC = lambdaC, censor = censor, ncores = ncores)
      } else {
        return(0)
      }
      for (k in 1:sim) {
        Model1[k] <- a[[k]][1]
        Model2[k] <- a[[k]][2]
      }
      Powertable[i, 2] <- m[i]
      Model1 <- as.logical(Model1)
      Model2 <- as.logical(Model2)
      Powertable[i, 3] <- sum(Model1, na.rm = TRUE)/sum(!is.na(Model1))
      Powertable[i, 4] <- sum(Model2, na.rm = TRUE)/sum(!is.na(Model2))
    }
    Powertable[, 1] <- n[j]
    Powertable <- as.data.frame(Powertable)
    colnames(Powertable) <- c("NofLine", "NofMice", "ANCOVArandom", "Coxrandom")
    SumPowertable <- rbind(SumPowertable, Powertable)
  }

  class(SumPowertable) <- "PowerTable"

  SumPowertable$beta <- beta
  SumPowertable$lambda <- lambda
  SumPowertable$nu <- nu
  SumPowertable$tau2 <- tau2
  SumPowertable$lambdaC <- lambdaC
  SumPowertable$nsim <- sim
  SumPowertable$sigma2 <- sigma2
  SumPowertable$censor <- censor
  SumPowertable$call <- match.call()

  return(SumPowertable)

}
