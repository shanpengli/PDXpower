##' @title A function to obtain a power table with the combination of
##' various number of PDX lines and number of individuals per PDX lines per treatment based on a preliminary dataset
##' @aliases PowANOVADat
##' @param data data.frame in which to interpret the variables named in the formula.
##' @param formula a two-sided linear formula object describing the fixed-effects part of the model,
##' with the response on the left of a ~ operator and the terms, separated by + operators, on the right.
##' @param random an one-sided formula of the form \code{~ x1 + ... + xn | ID}.
##' @param n an integer number to specify the number of PDX lines.
##' @param m an integer number to specify the number of individuals per PDX line per treatment.
##' @param sim Number of Monte Carlo samples to be generated. Default is 1000.
##' @param two.sided a logical value to indicate if a two-sided hypothesis testing is conducted. Default is TRUE.
##' @param alpha significance level. Default is 0.05.
##' @param fixed.effect logical value to indicate if a fixed effects only model is fitted. Default is FALSE.
##' @param digits digits the number of significant digits to use when printing.
##' @param ncores number of cores for parallel computation.
##' @return Object of \code{PowANOVADat} with elements
##' \item{beta}{the estimated treatment effect from the pilot data.}
##' \item{tau2}{the estimated inter-PDX variance from the pilot data.}
##' \item{sigma2}{the estimated random error variance from the pilot data.}
##' \item{PowTab}{the estimates of statistical power across \code{n} and \code{m}.}
##' @examples
##' \donttest{
##' require(PDXpower)
##' data(animals1)
##'PowTab <- PowANOVADat(data = animals1, formula = log(Y) ~ Tx, random = ~ 1|ID,
##'n = 3, m = 2, ncores = 1)
##'PowTab
##'plotpower(PowTab[[4]], ylim = c(0, 1))
##' }
##' @export

PowANOVADat <- function(data = NULL, formula = NULL, random = NULL,
                        n = NULL, m = NULL,
                        sim = 100, two.sided = TRUE, alpha = 0.05, fixed.effect = FALSE, digits = 4, ncores = NULL) {

  if (!is.data.frame(data))
    stop("This is not a date frame.")
  if (is.null(formula))
    stop("Please specify a formula for fitting an ANOVA mixed effects model.")

  ANOVA <- nlme::lme(fixed = formula, random = random, data = data)
  tab <- summary(ANOVA)
  beta <- tab$tTable[2, 1]
  sigma2 <- tab$sigma^2
  tau2 <- as.matrix(nlme::getVarCov(ANOVA))

  if (is.null(ncores)) ncores <- parallel::detectCores()

  fit <- PowerTable(n = n, m = m, beta = beta, sigma2 = sigma2,
                    tau2 = tau2,
                    distr = "normal", sim = sim,
                    two.sided = two.sided,
                    censor = FALSE,
                    print = "ANOVA",
                    alpha = alpha,
                    fixed.effect = fixed.effect,
                    ncores = ncores)

  result <- list(beta = beta, tau2 = tau2, sigma2 = sigma2, PowTab = fit)

  class(result) <- "PowANOVADat"

  cat("Parameter estimates based on the pilot data:\n")
  cat("Treatment effect (beta):", round(result$beta, digits), "\n")
  cat("Variance of random effect (tau2):", round(result$tau2, digits), "\n")
  cat("Random error variance (sigma2):", round(result$sigma2, digits), "\n\n")
  cat("Monte Carlo power estimate, calculated as the
  proportion of instances where the null hypothesis
  H_0: beta = 0 is rejected (n = number of PDX lines,
  m = number of animals per arm per PDX line,
  N = total number of animals for a given combination of n and m):\n")
  print(result$PowTab)
  return(result)

}
