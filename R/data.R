#' Simulated preliminary animals uncensored data
#'
#' @description The \code{animals1} data frame has 18 rows and 3 columns, with all uncensored observations.
#'
#' @format This data frame contains the following columns:
#'
#'   \describe{
#'
#'   \item{\code{ID}}{PDX line identifier.}
#'
#'   \item{\code{Y}}{time-to-event variable.}
#'
#'   \item{\code{Tx}}{treatment indicator. \code{0} denotes the placebo group and \code{1} the treatment group.}
#'
#'   }
#' @usage data(animals1)
#'
"animals1"

#' Simulated preliminary animals censored data
#'
#' @description The \code{animals2} data frame has 18 rows and 4 columns, with some censored observations.
#'
#' @format This data frame contains the following columns:
#'
#'   \describe{
#'
#'   \item{\code{ID}}{PDX line identifier.}
#'
#'   \item{\code{Y}}{time-to-event variable.}
#'
#'   \item{\code{Tx}}{treatment indicator. \code{0} denotes the placebo group and \code{1} the treatment group.}
#'
#'   \item{\code{status}}{event status. \code{0} denotes right-censoring and \code{1} the event occurs.}
#'
#'   }
#' @usage data(animals2)
#'
"animals2"

