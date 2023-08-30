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
                       round(x$Coxrandom, digits = digits))

    colnames(data) <- c("PDX lines", "Mice", "ANOVA", "Cox Frailty")
  } else if (print == "Cox-frailty") {
    data <- data.frame(x$NofLine, x$NofMice,
                       round(x$Coxrandom, digits = digits))

    colnames(data) <- c("PDX lines", "Mice", "Cox Frailty")
  } else {
    data <- data.frame(x$NofLine, x$NofMice,
                       round(x$ANCOVArandom, digits = digits))

    colnames(data) <- c("PDX lines", "Mice", "ANOVA")
  }


  cat("\nCall:\n", sprintf(format(paste(deparse(x$call, width.cutoff = 500), collapse = ""))), "\n\n")

  print(data)
}
