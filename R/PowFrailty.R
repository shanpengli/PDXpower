##' @title A function to obtain a power table with the combination of
##' various number of PDX lines and number of individuals per PDX lines per treatment based on a prior knowledge of median survival
##' @param ctl.med.surv a numeric value of the hypothesized medial survival in the control arm. Default is 2.4.
##' @param tx.med.surv a numeric value of the hypothesized medial survival in the treatment arm. Default is 4.8.
##' @param nu shape parameter of Weibull distribution for the baseline hazard. Default is 1, i.e., constant failure rate.
##' @param tau2 variance of PDOX line specific random effect. Default is 0.1.
##' @param n an integer number to specify the number of PDX lines.
##' @param m an integer number to specify the number of individuals per PDX line per treatment.
##' @param sim Number of Monte Carlo samples to be generated. Default is 1000.
##' @param censor logical value of whether a censoring distribution is considered in a data generation setting. Default is FALSE.
##' @param two.sided A logical value to indicate if a two-sided hypothesis testing is conducted. Default is TRUE.
##' @param ncores number of cores for parallel computation.
##' @examples
##' require(PDXpower)
##' PowTab <- PowFrailty(ctl.med.surv = 2.4, tx.med.surv = 4.8, sim = 100,
##' n = c(3, 5, 10), m = c(2, 3, 4))
##' PowTab
##' plotpower(PowTab, ylim = c(0, 1))
##'
##' @export
##'

PowFrailty <- function(ctl.med.surv = 2.4, tx.med.surv = 4.8, nu = 1, tau2 = 0.1,
                       n = NULL, m = NULL, sim = 1000, censor = FALSE,
                       two.sided = TRUE, ncores = NULL) {

  lambda <- log(2)^(1/nu)/ctl.med.surv
  beta <- log(ctl.med.surv/tx.med.surv)

  fit <- PowerTable(n = n, m = m, beta = beta, lambda = lambda, nu = nu,
                    tau2 = tau2, distr = "Weibull", sim = sim,
                    censor = censor, two.sided = two.sided, ncores = ncores)

  return(fit)

}
