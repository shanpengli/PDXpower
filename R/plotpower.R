##' @title A functon to generate a four-panel power curve under specified
##' number of PDX lines and number of individuals per PDX lines per treatment
##' @param objectObject of class 'PowerTable'.
##' @param ylim The limit of y axis.
##' @export
##'

plotpower <- function(object, ylim = c(0, 0.1)) {

  if (!inherits(object, "PowerTable"))
    stop("Use only with 'PowerTable' objects.\n")

  object <- data.frame(object$NofLine, object$NofMice,
                              object$ANCOVArandom,
                              object$Coxrandom)

  colnames(object) <- c("PDOX lines", "NofMice", "ANCOVArandom", "Coxrandom")

  object$NofMice <- as.factor(object$NofMice)

  p2 <- ggplot2::ggplot(object,
                        ggplot2::aes(x = `PDOX lines`, y = ANCOVArandom,
                                     group = NofMice,
                                     color = NofMice)) +
    ggplot2::geom_line() +
    ggplot2::ylab("Power for ANOVA") +
    ggplot2::xlab("Number of PDX lines") +
    ylim(ylim) +
    theme_bw()

  p4 <- ggplot2::ggplot(object,
                        ggplot2::aes(x = `PDOX lines`, y = Coxrandom,
                                     group = NofMice,
                                     color = NofMice)) +
    ggplot2::geom_line() +
    ggplot2::ylab("Power for Cox frailty") +
    ggplot2::xlab("Number of PDX lines") +
    ylim(ylim) +
    theme_bw()

  ggpubr::ggarrange(p2, p4, ncol = 2, nrow = 1, common.legend = TRUE,
                    legend = "right")

}
