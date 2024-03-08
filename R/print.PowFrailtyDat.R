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

  cat("Power analysis based on pilot data\n\n")
  cat("A Cox frailty model was fitted\n")
  cat("Summary of parameter estimates from the pilot data:\n")
  cat("Scale parameter (lambda):", round(x$lambda, digits), "\n")
  cat("Shape parameter (nu):", round(x$nu, digits), "\n")
  cat("Treatment effect (beta):", round(x$beta, digits), "\n")
  cat("Variance of random effect (tau2):", round(x$tau2, digits), "\n")
  cat("The above parameter estimates are used as priori for Monte Carlo data generation from a Cox frailty model to estimate the power.\n")
  cat("The estimated power for each combination of number of PDX lines (n) and number of mice per arm per PDX line (m) is calculated as the proportion of rejecting the null hypothesis beta = 0.\n")
  cat("The summary of power across all possible combinations of n and m is shown below.\n\n")
  print(x$PowTab)

}
