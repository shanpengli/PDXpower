##' @title A function to obtain a power table with the combination of
##' various number of PDX lines and number of individuals per PDX lines per treatment based on a preliminary dataset
##' @param data data.frame in which to interpret the variables named in the formula.
##' @param formula a two-sided linear formula object describing the fixed-effects part of the model,
##' with the response on the left of a ~ operator and the terms, separated by + operators, on the right.
##' @param random an one-sided formula of the form \code{~ x1 + ... + xn | ID}.
##' @param n an integer number to specify the number of PDX lines.
##' @param m an integer number to specify the number of individuals per PDX line per treatment.
##' @param sim Number of Monte Carlo samples to be generated. Default is 1000.
##' @param two.sided A logical value to indicate if a two-sided hypothesis testing is conducted. Default is TRUE.
##' @param ncores number of cores for parallel computation.
##' @examples
##' require(PDXpower)
##' data <- SimPDXdata(seed = 1000, n = 3, m = 3, beta = -0.8, tau2 = 0.1, sigma2 = 1,
##' distr = "normal", censor = FALSE)
##'
##'PowTab <- PowANOVADat(data = data, formula = log(Y) ~ Tx, random = ~ 1|ID,
##'n = c(3, 5, 10), m = c(2, 3, 4))
##'PowTab
##'plotpower(PowTab, ylim = c(0, 1))
##'
##' @export

PowANOVADat <- function(data = NULL, formula = NULL, random = NULL,
                        n = NULL, m = NULL,
                        sim = 100, two.sided = TRUE, ncores = NULL) {

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
                    ncores = ncores)
  return(fit)

}
