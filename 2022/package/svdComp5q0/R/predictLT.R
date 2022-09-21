#' Predict a Life Table from Child or Child/Adult Mortality.
#'
#' @description \code{svdComp5q0} predicts single-year, age-specific probabilities of dying (1qx) using the SVD-Comp mortality model indexed by child/child-adult mortality.
#' @author Samuel J. Clark, \email{work@samclark.net}
#' @references \url{https://arxiv.org/abs/1612.01408}
#'
#' @param sex Character: 'female' or 'male'.
#' @param cm Decimal: the input value(s) for 5q0; either a single value or a vector of values.
#' @param smooth Boolean: use either smooth or raw SVD-derived components. Default is TRUE.
#' @param outlogit Boolean: output either logit scale or natural scale 1qx values. Default is FALSE.
#' @param out5 Boolean: if returning natural scale values and out5=TRUE, then return in five-year age groups, 5qx. Default is TRUE.
#' @param am Optional decimal: input value(s) for 45q15; either single value or vector of values.  If a vector, must have the same number of elements as cm.
#' @param modsv Optional integer: specifies version of calibration models to use; defaults to 2022 but can be set to 2018.
#' @return Data frame: predicted 1qx values for ages 0:109. Age 110 assumed to be 1.0 and not returned. Columns labeled with input child mortality values.
#' @examples
#' predictLT("female",0.05)
#' predictLT("female",0.05,modsv=2018)
#' predictLT("male",0.03,am=0.26)
#' predictLT("male",0.03,TRUE,TRUE,TRUE,0.26)
#' predictLT("male",c(0.03,0.01))
#' \dontrun{predictLT("male",c(0.03,0.01),am=0.3)}
#' @importFrom stats predict
#' @export
predictLT <- function(sex,cm,smooth=TRUE,outlogit=FALSE,out5=TRUE,am=NULL,modsv=2022) {

  # sex: female or male
  # cm is a vector of 5q0 values
  # smooth: boolean, use smooth components or not
  # logit: boolean, use to return logit(qx) values
  # out5: boolean, output in 5-year age groups 5qx (only used if returning natural scale values)
  # am is a vector of 45q15 values - if am not supplied, then aml predicted from cml
  # modsv: model version is either 2018 or 2022, defaults to 2022

  # set mods, 2022 unless otherwise specified
  mods <- mods2022
  if (modsv == 2018) {mods <- mods2018}

  # ensure essential parameters have reasonable values
  if (missing(sex) | missing(cm))
    stop('<sex> (female or male) or <cm> (number between 0.003 and 0.3) does not have a value')
  if (!(tolower(sex) == "female" | tolower(sex) == "male") | !(all(cm >= 0.003) & all(cm <= 0.3)))
    stop('<sex> (female or male) or <cm> (number between 0.003 and 0.3) does not have a value')
  if (!missing(am)) {
    if(length(cm) != length(am))
      stop('<am> must be same length as <cm>')
  }

  # transform inputs
  cml <- logit(cm)
  cmls <- cml^2
  cmlc <- cml^3
  # if no adult mortality specified, predict adult mortality
  if(missing(am)) {
    preds.aml <- data.frame(
      cm = as.numeric(cm),
      cml = as.numeric(cml),
      cmls = as.numeric(cmls),
      cmlc = as.numeric(cmlc)
    )
    aml <- predict(mods[[sex]]$aml,newdata=preds.aml)
    am <- expit(aml)
  } else {
    aml <- logit(am)
  }
  amls <- aml^2
  amlc <- aml^3
  cmlaml <- cml*aml

  # predict vs using child and adult mortality
  preds.vs <- data.frame(
    cm = as.numeric(cm),
    cml = as.numeric(cml),
    cmls = as.numeric(cmls),
    cmlc = as.numeric(cmlc),
    am = as.numeric(am),
    amls = as.numeric(amls),
    amlc = as.numeric(amlc),
    cmlaml = as.numeric(cmlaml)
  )
  v1 <- predict(mods[[sex]]$v1,newdata=preds.vs)
  v2 <- predict(mods[[sex]]$v2,newdata=preds.vs)
  v3 <- predict(mods[[sex]]$v3,newdata=preds.vs)
  v4 <- predict(mods[[sex]]$v4,newdata=preds.vs)

  # pick either raw or smoothed components
  if (smooth) {
    cs <- mods[[sex]]$components.sm
  } else {
    cs <- mods[[sex]]$components
  }

  # calculate predicted qx values
  v <- cbind(v1,v2,v3,v4)
  r.p <- matrix(data=0,ncol=length(cml),nrow=dim(cs)[2])
  for (z in 1:4) {
    r.p <- r.p + cs[z,] %*% t(v[,z])
  }
  r.p <- r.p + mods[[sex]]$offset

  # predict q0 separately
  cmls <- cml^2
  preds.q0 <- data.frame(
    cml = as.numeric(cml),
    cmls = as.numeric(cmls)
  )
  r.p[1,] <- predict(mods[[sex]]$q0,newdata=preds.q0)

  # label the return object r.p
  r.p <- data.frame(r.p)
  cm.col <- format(round(cm, 3), nsmall = 3)
  am.col <- format(round(am, 3), nsmall = 3)
  colnames(r.p) <- paste("cm-",cm.col,".am-",am.col,sep="")
  rownames(r.p) <- mods[[sex]]$rownames

  # returns the matrix of predicted qx values
  if (outlogit) {
    if (out5) {
      q5 <- q1to5(expit(r.p))
      logit.q5 <- data.frame(logit(q5)[1:23,])
      rownames(logit.q5) <- rownames(q5)[1:23]
      colnames(logit.q5) <- colnames(r.p)
      return(logit.q5)
    }
    return(r.p)
  } else {
    if (out5) {
      return(q1to5(expit(r.p)))
    }
    return(expit(r.p))
  }

}



