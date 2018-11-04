
## This file illustrates the code that was used for estimating the log-quadratic model
## using the bi-weight procedure as described in the article's Appendix.  Here, the model 
## is fitted to female data only.  For the paper, this same procedure was applied to data
## for males alone and for both sexes combined.

#############################  Setup  #################################

# Source functions file
source("functions.R")

# Create labels for age vectors
ages.5x1 <- c("0","1-4",paste(seq(5,105,5),seq(9,109,5),sep="-"),"110+")
sexes <- c("Female","Male","Total")

# Import HMD-719 dataset (used for fitting the model)
HMD719 <- read.csv("../Data/HMD-719.csv")

# Create CountryPeriod vector (with constant width)
Period <- as.vector(HMD719[, "Period"])
Country <- as.vector(HMD719[, "Country"])
tmp <- ifelse(nchar(Country)==3, paste(Country, "    ", sep=""), 
	    ifelse(nchar(Country)==6, paste(Country, " "   , sep=""), Country))
CountryPeriod <- paste(tmp, Period, sep=" ")

# Extract some parts of HMD-719 dataset (females only)

# 5q0 (vector)
tmp1 <- HMD719[, "Sex"]=="Female" & HMD719[, "AgeGroup"]=="0"
tmp2 <- HMD719[, "Sex"]=="Female" & HMD719[, "AgeGroup"]=="5-9"
Q5.all.pf <- 1 - HMD719[tmp2, "lx"] / HMD719[tmp1, "lx"]

# 45q15 (vector)
tmp1 <- HMD719[, "Sex"]=="Female" & HMD719[, "AgeGroup"]=="15-19"
tmp2 <- HMD719[, "Sex"]=="Female" & HMD719[, "AgeGroup"]=="60-64"
QQ.all.pf <- 1 - HMD719[tmp2, "lx"] / HMD719[tmp1, "lx"]

names(Q5.all.pf) <- names(QQ.all.pf) <- CountryPeriod[tmp1]
 
# mx (matrix)
tmp <- HMD719[, "Sex"]=="Female"
mx5x5.all.pf <- array(HMD719[tmp, "mx"], dim=c(length(ages.5x1), length(unique(CountryPeriod))), dimnames=list(ages.5x1, unique(CountryPeriod)))

###############  Fit log-quadratic model using bi-weight method  ###################

# Note - the fitting method is illustrated here for females only

# Fit log-quadratic portion of model
x.f <- cbind(log(Q5.all.pf), log(Q5.all.pf)^2)
y.f <- t(log(mx5x5.all.pf))
bifit.f <- bifit(x.f, y.f, c=6)
beta.f <- bifit.f$coef
dimnames(beta.f)[[2]] <- ages.5x1
beta.f[, 2] <- NA
ax.f <- beta.f[1,]; bx.f <- beta.f[2,]; cx.f <- beta.f[3,]

# Compute residuals and fit SVD portion of model
yhat1.f <- cbind(1,x.f)%*%beta.f
dimnames(yhat1.f) <- dimnames(y.f)
resid1.f <- yhat1.f - y.f
svd.f <- svd(resid1.f[,-c(1,2)],1,1)
# Set values of vx=0 for ages 0, 1-4 and above 90
vx.f <- c(0, NA, svd.f$v); vx.f[20:24] <- 0
names(vx.f) <- ages.5x1

# Compare the estimated coefficients to Table 
round(cbind(ax.f, bx.f, cx.f, vx.f), 4)


