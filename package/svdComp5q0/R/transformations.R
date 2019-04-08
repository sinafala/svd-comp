#' Take the logit of a number, the inverse of expit.
#'
#' @param x A number in the range (0,1).
#' @return The logit of \code{x}.
#' @examples
#' logit(0.5)
#' @export
logit <- function(x) {
  return(log(x/(1-x)))
}

#' Take the expit of a number, the inverse of logit.
#'
#' @param x A number on the real line.
#' @return The expit of \code{x}.
#' @examples
#' expit(-5)
#' @export
expit <- function(x) {
  return(exp(x)/(1+exp(x)) )
}
