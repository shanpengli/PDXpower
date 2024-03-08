##' @title A function to obtain a power table with the combination of
##' various number of PDX lines and number of individuals per PDX lines per treatment.
##' @aliases PowerTable
##' @param n an integer number to specify the number of PDX lines.
##' @param m an integer number to specify the number of individuals per PDX line per treatment.
##' @param beta Treatment effect for the treated group.
##' @param tau2 variance of PDX line specific random effect.
##' @param alpha significance level. Default is 0.05.
##' @param lambda Scale parameter of Weibull distribution for the baseline hazard.
##' @param nu Shape parameter of Weibull distribution for the baseline hazard.
##' @param sigma2 Error variance of log survival time for both treatment groups.
##' @param two.sided A logical value to indicate if a two-sided hypothesis testing is conducted.
##' Default is TRUE.
##' @param sim Number of Monte Carlo samples to be generated. Default is 1000.
##' @param distr Distributional assumption of the simulated event time.
##' @param Ct a fixed time point when a study is designed to end for generating type 1 censoring data.
##' @param censor logical value of whether a censoring distribution is considered in a data generation setting. Default is TRUE.
##' @param print a string to indicate which model result to be printed.
##' If \code{print = "both"}, then the power curves of both models will be printed.
##' Otherwise, print a power curve from one of the two models by specifying
##' either \code{print = "ANOVA"} or \code{print = "Cox-frailty"}.
##' @param fixed.effect logical value to indicate if a fixed effects only model is fitted. Default is FALSE.
##' @param ncores number of cores for parallel computation.
##' @return Object of class \code{PowerTable} with elements
##' \item{NofLine}{the number of PDX line \code{n}.}
##' \item{NofMice}{the number of mice per arm per PDX line \code{m}.}
##' \item{ANOVArandom}{the proportion of rejecting null treatment effect by fitting a ANOVA mixed effects model.}
##' \item{Coxrandom}{the proportion of rejecting null treatment effect by fitting a Cox frailty model.}
##' \item{ANOVAfix}{the proportion of rejecting null treatment effect by fitting a ANOVA fixed effects model if \code{fixed.effects = TRUE}.}
##' \item{Coxfix}{the proportion of rejecting null treatment effect by fitting a Cox fixed effects model if \code{fixed.effects = TRUE}.}
##' \item{censoringrate}{the average censoring rate across all Monte Carlo replicates.}
##' \item{beta}{the pre-determined treatment effect.}
##' \item{lambda}{the pre-determined scale parameter of baseline hazard for the Cox frailty model.}
##' \item{nu}{the pre-determined shape parameter of baseline hazard for the Cox frailty model.}
##' \item{tau2}{the pre-determined inter-PDX variance.}
##' \item{Ct}{the pre-determined fixed time point to indicate the end of a study for type I censoring.}
##' \item{nsim}{total number of Monte Carlo replicates.}
##' \item{sigma2}{the pre-determined error variance for the ANOVA mixed effects model.}
##' \item{censor}{a logical value to indicate whether type I censoring mechanism is considered for simulation.}
##' \item{print}{a string to indicate which model is considered for simulation.}
##' \item{fixed.effect}{a logical value to indicate whether a fixed effects model is considered for simulation.}
##' \item{call}{match call.}
##' @examples
##' n <- 3
##' m <- 2
##' beta <- 0.8
##' lambda <- 0.3
##' nu <- 1
##' tau2 <- 0.1
##' \donttest{
##' fit <- PowerTable(n = n, m = m, beta = beta, lambda = lambda, nu = nu,
##'                   tau2 = tau2, distr = "Weibull", sim = 5,
##'                   censor = FALSE,
##'                   print = "both", ncores = 1)
##' plotpower(fit, ylim = c(0, 1))
##' }
##'
##' @seealso \code{\link{plotpower}}
##' @export
##'

PowerTable <- function(n, m, beta, tau2 = 0.5, alpha = 0.05, lambda = 0.03,
                       nu = 2, sigma2 = 1, two.sided = TRUE, distr = c("Weibull", "normal"),
                       Ct = 5, censor = TRUE, sim = 1000,
                       print = c("both", "ANOVA", "Cox-frailty"), fixed.effect = FALSE, ncores = NULL) {

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
      Model1.fixed <- vector()
      Model2.fixed <- vector()
      censor.rate <- vector()
      if (distr == "Weibull") {
        a <- simfit(sim = sim, n = n[j], m = m[i], beta = beta, tau2 = tau2,
                    alpha = alpha, lambda = lambda, nu = nu, distr = distr, two.sided = two.sided,
                    Ct = Ct, censor = censor, model = print, fixed.effect = fixed.effect, ncores = ncores)
      } else if (distr == "normal") {
        a <- simfit(sim = sim, n = n[j], m = m[i], beta = beta, tau2 = tau2,
                    alpha = alpha, sigma2 = sigma2, distr = distr, two.sided = two.sided,
                    Ct = Ct, censor = censor, model = print, fixed.effect = fixed.effect, ncores = ncores)
      } else {
        return(0)
      }

      Powertable[i, 2] <- m[i]

      if (print == "both") {

        if (fixed.effect) {
          for (k in 1:sim) {
            Model1.fixed[k] <- a[[k]]$Remodel1.fixed
            Model2.fixed[k] <- a[[k]]$Remodel2.fixed
            censor.rate[k] <- a[[k]]$censor.rate
          }
          Model1.fixed <- as.logical(Model1.fixed)
          Model2.fixed <- as.logical(Model2.fixed)
          Powertable[i, 5] <- sum(Model1.fixed, na.rm = TRUE)/sum(!is.na(Model1.fixed))
          Powertable[i, 6] <- sum(Model2.fixed, na.rm = TRUE)/sum(!is.na(Model2.fixed))
        } else {
          for (k in 1:sim) {
            Model1[k] <- a[[k]]$Remodel1
            Model2[k] <- a[[k]]$Remodel2
            censor.rate[k] <- a[[k]]$censor.rate
          }
          Model1 <- as.logical(Model1)
          Model2 <- as.logical(Model2)
          Powertable[i, 3] <- sum(Model1, na.rm = TRUE)/sum(!is.na(Model1))
          Powertable[i, 4] <- sum(Model2, na.rm = TRUE)/sum(!is.na(Model2))
        }

      } else if (print == "ANOVA") {

        if (fixed.effect) {
          for (k in 1:sim) {
            Model1.fixed[k] <- a[[k]]$Remodel1.fixed
            censor.rate[k] <- a[[k]]$censor.rate
          }
          Model1.fixed <- as.logical(Model1.fixed)
          Powertable[i, 5] <- sum(Model1.fixed, na.rm = TRUE)/sum(!is.na(Model1.fixed))
        } else {
          for (k in 1:sim) {
            Model1[k] <- a[[k]]$Remodel1
            censor.rate[k] <- a[[k]]$censor.rate
          }
          Model1 <- as.logical(Model1)
          Powertable[i, 3] <- sum(Model1, na.rm = TRUE)/sum(!is.na(Model1))
        }

      } else {

        if (fixed.effect) {
          for (k in 1:sim) {
            Model2.fixed[k] <- a[[k]]$Remodel2.fixed
          }
          Model2.fixed <- as.logical(Model2.fixed)
          Powertable[i, 6] <- sum(Model2.fixed, na.rm = TRUE)/sum(!is.na(Model2.fixed))
        } else {
          for (k in 1:sim) {
            Model2[k] <- a[[k]]$Remodel2
            censor.rate[k] <- a[[k]]$censor.rate
          }

          Model2 <- as.logical(Model2)
          Powertable[i, 4] <- sum(Model2, na.rm = TRUE)/sum(!is.na(Model2))
        }

      }
      Powertable[i, 7] <- mean(censor.rate)*100

    }
    Powertable[, 1] <- n[j]
    Powertable <- as.data.frame(Powertable)
    colnames(Powertable) <- c("NofLine", "NofMice", "ANOVArandom", "Coxrandom", "ANOVAfix", "Coxfix", "censoringrate")
    SumPowertable <- rbind(SumPowertable, Powertable)
  }

  class(SumPowertable) <- "PowerTable"

  SumPowertable$beta <- beta
  SumPowertable$lambda <- lambda
  SumPowertable$nu <- nu
  SumPowertable$tau2 <- tau2
  SumPowertable$Ct <- Ct
  SumPowertable$nsim <- sim
  SumPowertable$sigma2 <- sigma2
  SumPowertable$censor <- censor
  SumPowertable$print <- print
  SumPowertable$fixed.effect <- fixed.effect
  SumPowertable$call <- match.call()


  return(SumPowertable)

}
