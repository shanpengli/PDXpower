##' @title Print PowFrailtyDat
##' @name print.PowFrailtyDat
##' @aliases print.PowFrailtyDat
##' @param x object of class 'PowFrailtyDat'.
##' @param digits the number of significant digits to use when printing.
##' @param ... Further arguments passed to or from other methods.
##' @return a summary of power analysis including parameter estimates and statistical power.
##' @author Shanpeng Li \email{lishanpeng0913@ucla.edu}
##' @seealso \code{\link{PowFrailtyDat}}
##' @export
##'

print.PowFrailtyDat <- function(x, digits = 4, ...) {

  if (!inherits(x, "PowFrailtyDat"))
    stop("Use only with 'PowFrailtyDat' xs.\n")

  cat("Parameter estimates based on the pilot data:\n")
  cat("Scale parameter (lambda):", round(x$lambda, digits), "\n")
  cat("Shape parameter (nu):", round(x$nu, digits), "\n")
  cat("Treatment effect (beta):", round(x$beta, digits), "\n")
  cat("Variance of random effect (tau2):", round(x$tau2, digits), "\n")
  cat("Monte Carlo power estimate, calculated as the
  proportion of instances where the null hypothesis
  H_0: beta = 0 is rejected (n = number of PDX lines,
  m = number of animals per arm per PDX line,
  N = total number of animals for a given combination
  of n and m).\n")
  cat("The mean censoring rate for each combination of n and m is calculated across ", x$PowTab$nsim, " Monte Carlo samples.\n\n")
  print(x$PowTab)

}
