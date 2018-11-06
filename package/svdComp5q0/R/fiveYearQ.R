#' Convert Life Table Probabilities of Dying from 1-year to Standard 5-year Age Groups.
#'
#' @description \code{q1to5} converts 1-year life table probabilities of dying 1qx to standard 5-year age groups: 0, 1-4, 5-9, etc.
#' @author Samuel J. Clark, \email{work@samclark.net}
#'
#' @param q1 Decimal: the input values for 1qx; either a single vector or a matrix, age x life table.
#' @return Data frame: equivalent values for 5qx.
#' @examples
#' q1 <- svdComp5q0("female",0.08,out5=FALSE)
#' q1to5(q1)
q1to5 <- function(q1) {

  if (missing(q1))
    stop('q1 does not have a value')
  if (!nrow(q1) >= 109)
    stop('q1 must have at least 109 single-year age groups')

  # initialize matrix for five-year qs and vector for row names
  q5 <- matrix(data=0,ncol=ncol(q1),nrow=24)
  rNames <- rep("",24)

  # age 0
  q5[1,] <- as.matrix(q1[1,])
  rNames[1] <- "0"
  # ages 1-4
  tmp.q <- rep(1,ncol(q1))
  for (i in 2:5) {
    tmp.q <- tmp.q * (1-q1[i,])
  }
  q5[2,] <- as.matrix(1-tmp.q)
  rNames[2] <- "1-4"
  # five-year age groups for ages 5-105 (ending 110)
  for (j in 1:21) {
    tmp.q <- rep(1,ncol(q1))
    for (i in (j*5+1):(j*5+5)) {
      tmp.q <- tmp.q * (1-q1[i,])
    }
    q5[(j+2),] <- as.matrix(1-tmp.q)
    rNames[(j+2)] <- paste((j*5),"-",(j*5+4),sep="")
  }
  # last age 110 age group
  q5[24,] <- 1.0
  rNames[24] <- "110+"

  # return a data frame
  q5 <- data.frame(q5)

  # label rows and columns
  rownames(q5) <- rNames
  colnames(q5) <- colnames(q1)

  return(q5)

}
