##' @title A function to generate a four-panel power curve under specified
##' number of PDX lines and number of individuals per PDX lines per treatment
##' @param object of class 'PowerTable'.
##' @param ylim The limit of y axis.
##' @export
##'

plotpower <- function(object, ylim = c(0, 0.1)) {

  if (!inherits(object, "PowerTable"))
    stop("Use only with 'PowerTable' objects.\n")

  print <- object$print

  if (print == "both") {
    object <- data.frame(object$NofLine, object$NofMice,
                         object$ANCOVArandom,
                         object$Coxrandom)

    colnames(object) <- c("PDOX lines", "Number of mice", "ANCOVArandom", "Coxrandom")

    object$`Number of mice` <- as.factor(object$`Number of mice`)

    p2 <- ggplot2::ggplot(object,
                          ggplot2::aes(x = `PDOX lines`, y = ANCOVArandom,
                                       group = `Number of mice`,
                                       color = `Number of mice`)) +
      ggplot2::geom_line() +
      ggplot2::ylab("Power for ANOVA") +
      ggplot2::xlab("Number of PDX lines") +
      ylim(ylim) +
      theme_bw()

    p4 <- ggplot2::ggplot(object,
                          ggplot2::aes(x = `PDOX lines`, y = Coxrandom,
                                       group = `Number of mice`,
                                       color = `Number of mice`)) +
      ggplot2::geom_line() +
      ggplot2::ylab("Power for Cox frailty") +
      ggplot2::xlab("Number of PDX lines") +
      ylim(ylim) +
      theme_bw()

    ggpubr::ggarrange(p2, p4, ncol = 2, nrow = 1, common.legend = TRUE,
                      legend = "right")

  } else if (print == "ANOVA") {

    object <- data.frame(object$NofLine, object$NofMice,
                         object$ANCOVArandom)

    colnames(object) <- c("PDOX lines", "Number of mice", "ANCOVArandom")

    object$`Number of mice` <- as.factor(object$`Number of mice`)

    ggplot2::ggplot(object, ggplot2::aes(x = `PDOX lines`, y = ANCOVArandom,
                                       group = `Number of mice`,
                                       color = `Number of mice`)) +
      ggplot2::geom_line() +
      ggplot2::ylab("Power for ANOVA") +
      ggplot2::xlab("Number of PDX lines") +
      ylim(ylim) +
      theme_bw()

  } else if (print == "Cox-frailty") {

    object <- data.frame(object$NofLine, object$NofMice,
                         object$Coxrandom)

    colnames(object) <- c("PDOX lines", "Number of mice", "Coxrandom")

    object$`Number of mice` <- as.factor(object$`Number of mice`)

    ggplot2::ggplot(object,
                    ggplot2::aes(x = `PDOX lines`, y = Coxrandom,
                                       group = `Number of mice`,
                                       color = `Number of mice`)) +
      ggplot2::geom_line() +
      ggplot2::ylab("Power for Cox frailty") +
      ggplot2::xlab("Number of PDX lines") +
      ylim(ylim) +
      theme_bw()
  } else {

    stop("Please choose one of the following options for printing: both, ANOVA, Cox-frailty.")

  }

}
