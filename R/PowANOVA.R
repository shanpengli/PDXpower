##' @title A function to obtain a power table with the combination of
##' various number of PDX lines and number of individuals per PDX lines per treatment based on a prior knowledge of median survival
##' @aliases PowANOVA
##' @param ctl.med.surv a numeric value of the hypothesized medial survival in the control arm. Default is 2.4.
##' @param tx.med.surv a numeric value of the hypothesized medial survival in the treatment arm. Default is 4.8.
##' @param tau2 variance of PDOX line specific random effect. Default is 0.1.
##' @param sigma2 variance of random error.
##' @param n an integer number to specify the number of PDX lines.
##' @param m an integer number to specify the number of individuals per PDX line per treatment.
##' @param sim Number of Monte Carlo samples to be generated. Default is 1000.
##' @param two.sided A logical value to indicate if a two-sided hypothesis testing is conducted. Default is TRUE.
##' @param ncores number of cores for parallel computation.
##' @examples
##' \dontrun{
##' require(PDXpower)
##' PowTab <- PowANOVA(ctl.med.surv = 2.4, tx.med.surv = 4.8, sim = 5,
##' n = c(3, 5, 10), m = c(2, 3, 4))
##' PowTab
##' plotpower(PowTab, ylim = c(0, 1))
##' }
##' @export
##'

PowANOVA <- function(ctl.med.surv = 2.4, tx.med.surv = 4.8, tau2 = 0.1, sigma2 = 1,
                       n = NULL, m = NULL, sim = 100,
                       two.sided = TRUE, ncores = NULL) {

  beta <- log(ctl.med.surv/tx.med.surv)

  fit <- PowerTable(n = n, m = m, beta = beta, sigma2 = sigma2,
                    tau2 = tau2, distr = "normal", sim = sim,
                    censor = FALSE, two.sided = two.sided, print = "ANOVA",
                    ncores = ncores)

  return(fit)

}
