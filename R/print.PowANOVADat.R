##' @title Print PowANOVADat
##' @name print.PowANOVADat
##' @aliases print.PowANOVADat
##' @param x object of class 'PowANOVADat'.
##' @param digits the number of significant digits to use when printing.
##' @param ... Further arguments passed to or from other methods.
##' @author Shanpeng Li \email{lishanpeng0913@ucla.edu}
##' @seealso \code{\link{PowANOVADat}}
##' @export
##'

print.PowANOVADat <- function(x, digits = 4, ...) {

  if (!inherits(x, "PowANOVADat"))
    stop("Use only with 'PowANOVADat' xs.\n")

  cat("Power analysis based on pilot data\n\n")
  cat("An ANOVA mixed effects model was fitted\n")
  cat("Summary of parameter estimates from the data:\n")
  cat("Treatment effect (beta):", round(x$beta, digits), "\n")
  cat("Variance of random effect (tau2):", round(x$tau2, digits), "\n")
  cat("Random error variance (sigma2):", round(x$sigma2, digits), "\n")
  cat("The above parameters are used for Monte Carlo data generation from a ANOVA mixed effects model.\n")
  cat("Power for each combination of number of PDX lines (n) and number of mice per arm per PDX line (m) is calculated based on the proportion of rejecting the null hypothesis beta = 0.\n")

  print(x$PowTab)

}
