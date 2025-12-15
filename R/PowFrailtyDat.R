##' @title A function to obtain a power table with the combination of
##' various number of PDX lines and number of individuals per PDX lines per treatment based on a preliminary dataset
##' @aliases PowFrailtyDat
##' @param data data.frame in which to interpret the variables named in the formula.
##' @param formula a formula object, with the response on the left of a ~ operator, and the terms on the right.
##' The response must be a survival object as returned by the Surv function.
##' @param maxit maximum number of iterations needed for model fitting. Default is 50.
##' @param hazard distributional assumption of the baseline hazard. Default is Weibull.
##' @param n an integer number to specify the number of PDX lines.
##' @param m an integer number to specify the number of individuals per PDX line per treatment.
##' @param sim Number of Monte Carlo samples to be generated. Default is 1000.
##' @param censor logical value of whether a censoring distribution is considered in a data generation setting. Default is FALSE.
##' @param Ct a fixed time point when a study is designed to end for generating type 1 censoring data.
##' @param two.sided A logical value to indicate if a two-sided hypothesis testing is conducted. Default is TRUE.
##' @param alpha significance level. Default is 0.05.
##' @param digits digits the number of significant digits to use when printing.
##' @param ncores number of cores for parallel computation.
##' @return Object of \code{PowFrailtyDat} with elements
##' \item{lambda}{the estimated scale parameter of Weibull baseline hazard from the pilot data.}
##' \item{nu}{the estimated shape parameter of Weibull baseline hazard from the pilot data.}
##' \item{beta}{the estimated treatment effect from the pilot data.}
##' \item{tau2}{the estimated inter-PDX variance from the pilot data.}
##' \item{sigma2}{the estimated random error variance from the pilot data.}
##' \item{PowTab}{the estimates of statistical power across \code{n} and \code{m}.}
##' @examples
##' \donttest{
##' require(PDXpower)
##' require(frailtypack)
##' data(animals2)
##' PowTab <- PowFrailtyDat(data = animals2,
##'                         formula = survival::Surv(Y,status) ~ Tx + cluster(ID),
##'                         n = 3, m = 4,
##'                         Ct = 12, censor = TRUE,
##'                         sim = 20, ncores = 1)
##' plotpower(PowTab[[5]], ylim = c(0, 1))
##' }
##' @export

PowFrailtyDat <- function(data = NULL, formula = NULL, maxit = 50, hazard = "Weibull",
                          n = NULL, m = NULL, sim = 1000, censor = FALSE, Ct = 5,
                          two.sided = TRUE, alpha = 0.05, digits = 4, ncores = NULL) {

  if (!is.data.frame(data))
    stop("This is not a date frame.")
  if (is.null(formula))
    stop("Please specify a survival formula for fitting a Cox fraility model.")

  fit <- frailtypack::frailtyPenal(formula, data=data, RandDist = "LogN",
                                   print.times = FALSE, maxit = maxit, hazard = hazard)

  lambda <- fit$scale.weib[1]^(-fit$shape.weib[1])
  nu <- fit$shape.weib[1]
  beta <- fit$coef
  tau2 <- fit$sigma2

  if (is.null(ncores)) ncores <- parallel::detectCores()

  fit <- PowerTable(n = n, m = m, beta = beta, lambda = lambda, nu = nu,
                    tau2 = tau2, distr = hazard, sim = sim,
                    censor = censor,
                    two.sided = two.sided, Ct = Ct,
                    print = "Cox-frailty",
                    alpha = alpha,
                    ncores = ncores)

  result <- list(lambda = lambda, nu = nu, beta = beta, tau2 = tau2, PowTab = fit)

  class(result) <- "PowFrailtyDat"

  cat("Parameter estimates based on the pilot data:\n")
  cat("Scale parameter (lambda):", round(result$lambda, digits), "\n")
  cat("Shape parameter (nu):", round(result$nu, digits), "\n")
  cat("Treatment effect (beta):", round(result$beta, digits), "\n")
  cat("Variance of random effect (tau2):", round(result$tau2, digits), "\n\n")
  cat("Monte Carlo power estimate, calculated as the
  proportion of instances where the null hypothesis
  H_0: beta = 0 is rejected (n = number of PDX lines,
  m = number of animals per arm per PDX line,
  N = total number of animals for a given combination
  of n and m,
  Censoring Rate = average censoring rate across 500
  Monte Carlo samples):\n")
  print(result$PowTab)
  return(result)

}
