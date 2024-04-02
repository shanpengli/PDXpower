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

  cat("Parameter estimates based on the pilot data:\n")
  cat("Treatment effect (beta):", round(x$beta, digits), "\n")
  cat("Variance of random effect (tau2):", round(x$tau2, digits), "\n")
  cat("Random error variance (sigma2):", round(x$sigma2, digits), "\n")
  cat("Monte Carlo power estimate, calculated as the
  proportion of instances where the null hypothesis
  H_0: beta = 0 is rejected (n = number of PDX lines,
  m = number of animals per arm per PDX line,
  N = total number of animals for a given combination of n and m).\n\n")
  print(x$PowTab)

}
