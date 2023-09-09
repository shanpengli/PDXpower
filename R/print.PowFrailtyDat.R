##' @title Print PowFrailtyDat
##' @name print.PowFrailtyDat
##' @aliases print.PowFrailtyDat
##' @param x object of class 'PowFrailtyDat'.
##' @param digits the number of significant digits to use when printing.
##' @param ... Further arguments passed to or from other methods.
##' @author Shanpeng Li \email{lishanpeng0913@ucla.edu}
##' @seealso \code{\link{PowFrailtyDat}}
##' @export
##'

print.PowFrailtyDat <- function(x, digits = 4, ...) {

  if (!inherits(x, "PowFrailtyDat"))
    stop("Use only with 'PowFrailtyDat' xs.\n")

  cat("Power analysis based on pilot data\n\n")
  cat("A Cox frailty model was fitted\n")
  cat("Summary of parameter estimates from the data:\n")
  cat("Scale parameter (lambda):", round(x$lambda, digits), "\n")
  cat("Shape parameter (nu):", round(x$nu, digits), "\n")
  cat("Treatment effect (beta):", round(x$beta, digits), "\n")
  cat("Variance of random effect (alpha):", round(x$tau2, digits), "\n")
  print(x$PowTab)

}
