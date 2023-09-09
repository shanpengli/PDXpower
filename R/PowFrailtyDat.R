##' @title A function to obtain a power table with the combination of
##' various number of PDX lines and number of individuals per PDX lines per treatment based on a preliminary dataset
##' @param data data.frame in which to interpret the variables named in the formula.
##' @param formula a formula object, with the response on the left of a ~ operator, and the terms on the right.
##' The response must be a survival object as returned by the Surv function.
##' @param maxit maximum number of iterations needed for model fitting. Default is 50.
##' @param hazard distributional assumption of the baseline hazard. Default is Weibull.
##' @param n an integer number to specify the number of PDX lines.
##' @param m an integer number to specify the number of individuals per PDX line per treatment.
##' @param sim Number of Monte Carlo samples to be generated. Default is 1000.
##' @param censor logical value of whether a censoring distribution is considered in a data generation setting. Default is FALSE.
##' @param two.sided A logical value to indicate if a two-sided hypothesis testing is conducted. Default is TRUE.
##' @param ncores number of cores for parallel computation.
##' @examples
##' require(PDXpower)
##' data(mice)
##'
##'PowTab <- PowFrailtyDat(data = mice, formula = Surv(Y,status) ~ Tx + cluster(ID),
##'n = c(3, 5, 10), m = c(2, 3, 4))
##'PowTab
##'plotpower(PowTab[[5]], ylim = c(0, 1))
##'
##' @export

PowFrailtyDat <- function(data = NULL, formula = NULL, maxit = 50, hazard = "Weibull",
                          n = NULL, m = NULL, sim = 1000, censor = FALSE, two.sided = TRUE, ncores = NULL) {

  if (!is.data.frame(data))
    stop("This is not a date frame.")
  if (is.null(formula))
    stop("Please specify a survival formula for fitting a Cox fraility model.")
  CoxRandom <- frailtypack::frailtyPenal(formula,
                                         data=data, RandDist = "LogN",
                                         print.times = FALSE, maxit = maxit, hazard = hazard)

  lambda <- CoxRandom$scale.weib[1]^(-CoxRandom$shape.weib[1])
  nu <- CoxRandom$shape.weib[1]
  beta <- CoxRandom$coef
  tau2 <- CoxRandom$sigma2

  if (is.null(ncores)) ncores <- parallel::detectCores()

  fit <- PowerTable(n = n, m = m, beta = beta, lambda = lambda, nu = nu,
                    tau2 = tau2, distr = hazard, sim = sim,
                    censor = censor,
                    two.sided = two.sided,
                    print = "Cox-frailty",
                    ncores = ncores)

  result <- list(lambda = lambda, nu = nu, beta = beta, tau2 = tau2, PowTab = fit)

  class(result) <- "PowFrailtyDat"

  return(result)

}
