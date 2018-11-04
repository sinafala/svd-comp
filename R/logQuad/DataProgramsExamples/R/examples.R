
setwd("/Users/samueljclark/Desktop/Log Quad Mx Model/DataProgramsExamples/R/")

#############################  Setup  #################################

# Source functions file
source("functions.R")

# Create labels for age vectors
ages.5x1 <- c("0","1-4",paste(seq(5,105,5),seq(9,109,5),sep="-"),"110+")
sexes <- c("Female","Male","Total")

# Import matrix of model coefficients
tmp1 <- read.csv("../Data/coefs.logquad.HMD719.csv")
tmp2 <- array(c(as.matrix(tmp1[, 3:6])), dim=c(24, 3, 4), dimnames=list(ages.5x1, sexes, c("ax", "bx", "cx", "vx")))
coefs <- aperm(tmp2, c(1,3,2))

#########################  Simple examples  #############################

# Examples of lthat.any2.logquad() with various choices of 2 input parameters

# (1) Using 5q0 and k
ex1 <- lthat.any2.logquad(coefs, "Female", Q5=0.05, k=0)
ex1$lt       # life table (rounded values)
ex1$lt.exact # life table (calculated values)
ex1$Q5       # 5q0
ex1$k        # k parameter of log-quadratic model
ex1$e0       # life expectancy
ex1$QQa      # 45q15
ex1$QQb      # 35q15
ex1$Q1       # 1q0
ex1$mx1x1    # mortality rates for 1-year age intervals

# (2) Using 5q0 and 45q15
ex2 <- lthat.any2.logquad(coefs, "Female", Q5=0.05, QQa=0.2)

# (3) Using 1q0 and 45q15
ex3 <- lthat.any2.logquad(coefs, "Female", Q1=0.05, QQa=0.2)

# (4) Using 5q0 and 35q15
ex4 <- lthat.any2.logquad(coefs, "Female", Q5=0.05, QQb=0.125)

# (5) Using 5q0 and e0
ex5 <- lthat.any2.logquad(coefs, "Female", Q5=0.05, e0=65)

# (6) Using 45q15 and e0
ex6 <- lthat.any2.logquad(coefs, "Female", QQa=0.2, e0=65)

##################  A more complicated example  ####################

# Age patterns based on three combinations of 2 input parameters:
# Case A:  Q5 (constant) and k  (variable)
# Case B:  e0 (constant) and Q5 (variable)
# Case C:  k  (constant) and e0 (variable)
# Compare to Figure 4 of the Population Studies article

plt <- c("black","black","white")
palette(plt)
fol <- "../Figures/"
for (sex in sexes) for (case in c("A", "B", "C")) {
	print(c(sex, case))
	tmp <- paste(fol, "fig4.", substr(sex,1,1), ".", case, ".png", sep="")
	png(file=tmp, width=720, height=630)   # good size for 3 per column print
	if (case=="A") { Q5 <- rep(0.05,       length=9); k  <- 4:-4                 ; e0 <- NULL; QQa <- NULL; QQb <- NULL; Q1 <- NULL }
	if (case=="B") { Q5 <- seq(0.06, 0.14, length=9); e0 <- rep(60,     length=9); k  <- NULL; QQa <- NULL; QQb <- NULL; Q1 <- NULL }
	if (case=="C") { k  <- rep(0   ,       length=9); e0 <- seq(38, 78, length=9); Q5 <- NULL; QQa <- NULL; QQb <- NULL; Q1 <- NULL }
	tmp  <- lthat.any2.logquad(coefs, sex, Q5=Q5, k=k, e0=e0, QQa=QQa, QQb=QQb, Q1=Q1)
	Q5 <- tmp$Q5; k <- tmp$k; e0 <- tmp$e0; QQa <- tmp$QQa; QQb <- tmp$QQb; Q1 <- tmp$Q1
	legend.txt <- if (case=="A") paste("k = "  , k , ", e0 = " , round(tmp$e0,1), sep="") else
	              if (case=="B") paste("5q0 = ", Q5, ", k = "  , round(tmp$k ,2), sep="") else
	              if (case=="C") paste("e0 = " , e0, ", 5q0 = ", round(tmp$Q5,3), sep="")
	text.txt <- if (case=="A") paste(sex,"\n 5q0 = " , Q5[1], "\n     k = ", min(k) , ", ... , ", max(k) , sep="") else
	            if (case=="B") paste(sex,"\n   e0 = ", e0[1], "\n 5q0 = "  , min(Q5), ", ... , ", max(Q5), sep="") else
	            if (case=="C") paste(sex,"\n   k = " , k[1] , "\n e0 = "   , min(e0), ", ... , ", max(e0), sep="")
	x2 <- 0.5:110.5; y2 <- tmp$mx1x1
	plot.mx(x2, y2, legend.txt, text.txt)
	dev.off() }

