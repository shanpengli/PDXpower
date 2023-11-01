##' @title Print PowerTable
##' @name print
##' @aliases print.PowerTable
##' @param x object of class 'PowerTable'.
##' @param digits the number of significant digits to use when printing.
##' @param ... Further arguments passed to or from other methods.
##' @author Shanpeng Li \email{lishanpeng0913@ucla.edu}
##' @seealso \code{\link{PowerTable}}
##' @export
##'

print.PowerTable <- function(x, digits = 2, ...) {

  if (!inherits(x, "PowerTable"))
    stop("Use only with 'PowerTable' xs.\n")

  print <- x$print

  if (print == "both") {
    data <- data.frame(x$NofLine, x$NofMice,
                       round(x$ANCOVArandom, digits = digits),
                       round(x$Coxrandom, digits = digits),
                       round(x$censoringrate, digits = digits))

    colnames(data) <- c("n", "m", "ANOVA", "Cox Frailty", "Censoring Rate")
  } else if (print == "Cox-frailty") {
    data <- data.frame(x$NofLine, x$NofMice,
                       round(x$Coxrandom*100, digits = digits),
                       round(x$censoringrate, digits = digits))

    colnames(data) <- c("n", "m", "Power (%)", "Censoring Rate")
  } else {
    data <- data.frame(x$NofLine, x$NofMice,
                       round(x$ANCOVArandom*100, digits = digits))

    colnames(data) <- c("n", "m", "Power (%)")
  }


  cat("\nCall:\n", sprintf(format(paste(deparse(x$call, width.cutoff = 500), collapse = ""))), "\n\n")

  print(data)
}
