##' @title A function to obtain a power table with the combination of
##' various number of PDX lines and number of individuals per PDX lines per treatment based on a prior knowledge of median survival
##' @aliases PowFrailty
##' @param ctl.med.surv a numeric value of the hypothesized medial survival in the control arm. Default is 2.4.
##' @param tx.med.surv a numeric value of the hypothesized medial survival in the treatment arm. Default is 4.8.
##' @param nu shape parameter of Weibull distribution for the baseline hazard. Default is 1, i.e., constant failure rate.
##' @param tau2 variance of PDX line specific random effect. Default is 0.1.
##' @param n an integer number to specify the number of PDX lines.
##' @param m an integer number to specify the number of individuals per PDX line per treatment.
##' @param sim Number of Monte Carlo samples to be generated. Default is 1000.
##' @param censor logical value of whether a censoring distribution is considered in a data generation setting. Default is FALSE.
##' @param Ct a fixed time point when a study is designed to end for generating type 1 censoring data.
##' @param two.sided A logical value to indicate if a two-sided hypothesis testing is conducted. Default is TRUE.
##' @param alpha significance level. Default is 0.05.
##' @param ncores number of cores for parallel computation.
##' @return Object of \code{PowFrailty} with elements
##' \item{PowTab}{the estimates of statistical power across \code{n} and \code{m}.}
##' @examples
##' \donttest{
##' require(PDXpower)
##' PowTab <- PowFrailty(ctl.med.surv = 2.4,
##' tx.med.surv = 7.2, nu = 1, tau2 = 0.1, sim = 20,
##' censor = TRUE, Ct = 12, n = 3, m = 4, ncores = 1)
##' plotpower(PowTab, ylim = c(0, 1))
##' }
##' @export
##'

PowFrailty <- function(ctl.med.surv = 2.4, tx.med.surv = 4.8, nu = 1, tau2 = 0.1,
                       n = NULL, m = NULL, sim = 1000, censor = FALSE, Ct = 5,
                       two.sided = TRUE, alpha = 0.05, ncores = NULL) {

  lambda <- log(2)^(1/nu)/ctl.med.surv
  beta <- log(ctl.med.surv/tx.med.surv)

  PowTab <- PowerTable(n = n, m = m, beta = beta, lambda = lambda, nu = nu,
                    tau2 = tau2, distr = "Weibull", sim = sim,
                    censor = censor, two.sided = two.sided, print = "Cox-frailty",
                    alpha = alpha, Ct = Ct,
                    ncores = ncores)

  cat("Treatment effect (beta):", beta, "\n")
  cat("Scale parameter (lambda):", lambda, "\n")
  cat("Shape parameter (nu):", nu, "\n")
  cat("Variance of random effect (tau2):", tau2, "\n\n")
  cat("Monte Carlo power estimate, calculated as the
  proportion of instances where the null hypothesis
  H_0: beta = 0 is rejected (n = number of PDX lines,
  m = number of animals per arm per PDX line,
  N = total number of animals for a given combination
  of n and m,
  Censoring Rate = average censoring rate across 500
  Monte Carlo samples):\n")
  print(PowTab)
  return(PowTab)

}
