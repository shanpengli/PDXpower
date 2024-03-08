##' @title A function to obtain a power table with the combination of
##' various number of PDX lines and number of individuals per PDX lines per treatment based on a prior knowledge of median survival
##' @aliases PowANOVA
##' @param ctl.med.surv a numeric value of the hypothesized medial survival in the control arm. Default is 2.4.
##' @param tx.med.surv a numeric value of the hypothesized medial survival in the treatment arm. Default is 4.8.
##' @param tau2 variance of PDX line specific random effect. Default is 0.1.
##' @param sigma2 variance of random error.
##' @param n an integer number to specify the number of PDX lines.
##' @param m an integer number to specify the number of individuals per PDX line per treatment.
##' @param sim Number of Monte Carlo samples to be generated. Default is 1000.
##' @param two.sided A logical value to indicate if a two-sided hypothesis testing is conducted. Default is TRUE.
##' @param alpha significance level. Default is 0.05.
##' @param fixed.effect logical value to indicate if a fixed effects only model is fitted. Default is FALSE.
##' @param ncores number of cores for parallel computation.
##' @return Object of \code{PowANOVA} with elements
##' \item{PowTab}{the estimates of statistical power across \code{n} and \code{m}.}
##' @examples
##' \donttest{
##' require(PDXpower)
##' PowTab <- PowANOVA(ctl.med.surv = 2.4, tx.med.surv = 4.8, sim = 5,
##' n = 3, m = 2, ncores = 1)
##' PowTab
##' plotpower(PowTab, ylim = c(0, 1))
##' }
##' @export
##'

PowANOVA <- function(ctl.med.surv = 2.4, tx.med.surv = 4.8, tau2 = 0.1, sigma2 = 1,
                       n = NULL, m = NULL, sim = 100,
                       two.sided = TRUE, alpha = 0.05, fixed.effect = FALSE, ncores = NULL) {

  beta <- log(ctl.med.surv/tx.med.surv)

  PowTab <- PowerTable(n = n, m = m, beta = beta, sigma2 = sigma2,
                    tau2 = tau2, distr = "normal", sim = sim,
                    censor = FALSE, two.sided = two.sided, print = "ANOVA",
                    alpha = alpha,
                    fixed.effect = fixed.effect,
                    ncores = ncores)

  return(PowTab)

}
