##' @title Print PowANOVADat
##' @name print.PowANOVADat
##' @aliases print.PowANOVADat
##' @param x object of class 'PowANOVADat'.
##' @param digits the number of significant digits to use when printing.
##' @param ... Further arguments passed to or from other methods.
##' @return a summary of power analysis including parameter estimates and statistical power.
##' @author Shanpeng Li \email{lishanpeng0913@ucla.edu}
##' @seealso \code{\link{PowANOVADat}}
##' @export
##'

print.PowANOVADat <- function(x, digits = 4, ...) {

  if (!inherits(x, "PowANOVADat"))
    stop("Use only with 'PowANOVADat' xs.\n")

  cat("Power analysis based on pilot data\n\n")
  cat("A mixed effects ANOVA model was fitted\n")
  cat("Summary of parameter estimates from the pilot data:\n")
  cat("Treatment effect (beta):", round(x$beta, digits), "\n")
  cat("Variance of random effect (tau2):", round(x$tau2, digits), "\n")
  cat("Random error variance (sigma2):", round(x$sigma2, digits), "\n")
  cat("The above parameter estimates are used as priori for Monte Carlo data generation from a mixed effects ANOVA model to estimate the power.\n")
  cat("The estimated power for each combination of number of PDX lines (n) and number of mice per arm per PDX line (m) is calculated as the proportion of rejecting the null hypothesis beta = 0.\n")
  cat("The summary of power across all possible combinations of n and m is shown below.\n")
  cat("N denotes the total number of mice given a fixed n and m.\n\n")
  print(x$PowTab)

}
