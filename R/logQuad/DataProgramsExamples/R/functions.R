
bifit <- function(x, y, c=6, tol=1e-6, intercept = TRUE, tolerance = 1e-07, yname = NULL) {
	# Uses lsfit() to compute WLS using Tukey's biweight function
	# c is the tuning constant, typically around 6 to 9
	# tol is for convergence of the biweight fit; tolerance is for the matrix decomposition in lsfit
	# Like lsfit, input x matrix should not include a column of ones
	# Like lsfit, input y can be matrix if there are multiple left-hand sides
	
	if (is.vector(y)) {
		coef.old <- iter <- 0; coef.new <- 1; wt <- NULL
		while (max(abs(coef.new-coef.old)) > tol) {
			iter <- iter+1; print(iter)
			z <- lsfit(x, y, wt, intercept=intercept, tolerance=tolerance, yname=yname)
			S <- median(abs(z$resid))
			u <- z$resid / (c*S)
			wt <- ifelse(abs(u)<1, (1-u^2)^2, 0)
			coef.old <- coef.new
			coef.new <- z$coef }
		resid <- z$resid
		names(resid) <- names(wt) <- names(u) <- names(y)
		names(coef.new) <- paste("b", 0:(ncol(cbind(1,x))-1), sep="")
		return(list(coef=coef.new, residuals=z$resid, intercept=intercept, wt=wt, u=u)) }

	if (is.matrix(y)) {
		resid <- wt <- u <- coef <- NULL
		for (j in 1:ncol(y)) {
			print(paste("Y",j,":",sep=""))
			z <- bifit(x, y[,j], c=c, tol=tol, intercept=intercept, tolerance=tolerance, yname=yname)
			resid <- cbind(resid, z$resid)
			wt <- cbind(wt, z$wt)
			u <- cbind(u, z$u)
			coef <- cbind(coef, z$coef) }
		dimnames(resid) <- dimnames(wt) <- dimnames(u) <- dimnames(y)
		dimnames(coef) <- list(paste("b",0:(ncol(cbind(1,x))-1),sep=""),dimnames(y)[[2]])
		return(list(coef=coef, residuals=resid, intercept=intercept, wt=wt, u=u)) } }

kcalc <- function(coefs, sex, Q5, QQ) {
	# Compute value of k to match QQ=45q15
	if (length(Q5)!=length(QQ)) { print("error: length of Q5 and QQ input variables not equal"); break }
	ages <- paste(seq(15,55,5),seq(19,59,5),sep="-")
	vx <- coefs[, "vx", sex]
	if (length(Q5)==1) {
		mxhat1 <- mxhat.logquad(coefs, sex, Q5, k=0)
		tmp.fcn <- function(k, QQ, vx, mx) log(1-QQ) + sum(5*mx*exp(k*vx)) 
		tmp <- uniroot(tmp.fcn, c(-10,10), QQ=QQ, vx=vx[ages], mx=mxhat1[ages])
		k <- tmp$root }
	else {
		k <- Q5
		for (j in 1:length(Q5)) k[j] <- kcalc(Q5[j], sex, QQ[j], coefs) }
	return(k) }

mxhat.logquad <- function(coefs, sex, Q5, k=rep(0,length(Q5))) {
	# Inputs:   Q5, sex, k (optional; default is k=0), and model coefficients
	#           Q5 and k can be scalar or vector, but must have same length
	#           coefs contains coefficients ax, bx, cx, and vx
	# Outputs:  Vector (or matrix) of predicted sex-specific mx values
	#           with age groups 0, 1-4, 5-9, ..., 110+
	ages <- c("0", "1-4", paste(seq(5,105,5), seq(9,109,5), sep="-"), "110+")
	if (length(Q5)!=length(k)) { print("error: Q5 and k input vectors must have same length"); break }
	if (!is.array(coefs)) { print("Error: table of coefficients must be an array"); break }
	ax <- coefs[, "ax", sex]; bx <- coefs[, "bx", sex]; cx <- coefs[, "cx", sex]; vx <- coefs[, "vx", sex]
	h <- log(Q5)
	if (length(Q5)==1) {
		# Compute age-specific mx from h, h^2, and coefficients
		mx <- exp(ax + bx*h + cx*h^2 + vx*k)
		# Force 4q1 (and thus 4m1) to be consistent with 1q0 and 5q0
		a1 <- coale.demeny.a0 (mx[1], sex)
		a4 <- coale.demeny.4a1(mx[1], sex)
		Q1 <- mx[1] / ( 1 + (1-a4)*mx[1] )
		Q4 <- 1 - (1-Q5)/(1-Q1)
		mx[2] <- Q4 / ( 4 - (4-a4)*Q4 ) }
	else {
		mx <- matrix(NA, length(ages), length(Q5))
		dimnames(mx) <- list(ages, names(Q5))
		for (j in 1:length(Q5)) mx[,j] <- mxhat.logquad(coefs, sex, Q5[j], k[j]) }
	return(mx) }

lthat.logquad <- function(coefs, sex, Q5, k=rep(0,length(Q5))) {
	# Inputs:   Q5, sex, k (optional; default is k=0), and model coefficients
	#           Q5 and k can be scalar or vector (if k is specified, must have same length as Q5)
	# Outputs:  Matrix (or 3-way array) of predicted sex-specific life table(s)
	#           with age groups 0, 1-4, 5-9, ..., 110+
	if (length(Q5)!=length(k)) { print("error: Q5 and k input vectors must have same length"); break }
	ages <- c("0", "1-4", paste(seq(5,105,5),seq(9,109,5),sep="-"), "110+")
	w <- length(ages)
	nx <- c(1,4,rep(5,21),Inf); names(nx) <- ages
	if (length(Q5)==1) {
		mx <- mxhat.logquad(coefs, sex, Q5, k)
		tmp <- lt.from.mx(mx, sex, Q5=Q5)
		lt <- tmp$lt
		lt.exact <- tmp$lt.exact }
	else {
		lt <- lt.exact <- array(NA,c(length(ages), 8, length(Q5)))
		dimnames(lt) <- dimnames(lt.exact) <- list(ages,c("mx","qx","ax","lx","dx","Lx","Tx","ex"), names(Q5))
		for (j in 1:length(Q5)) {
			tmp <- lthat.logquad(coefs, sex, Q5[j], k[j])
			lt[,,j] <- tmp$lt; lt.exact[,,j] <- tmp$lt.exact } }
	return(list(lt=lt, lt.exact=lt.exact, Q5=Q5, k=k)) }

lthat.any2.logquad <- function(coefs, sex, Q5=NULL, k=NULL, e0=NULL, QQa=NULL, QQb=NULL, Q1=NULL, tol=1e-8, maxit=100) {
	# Inputs:   Any two of the following:  Q5=5q0, k, e0, QQa=45q15, QQb=35q15, and Q1=1q0 (except Q1 and Q5, or QQa and QQb)
	#           (Note:  elsewhere we use QQ=45q15; here we distinguish between QQa=45q15 and QQb=35q15)
	#           sex ("Female", "Male", or "Total")
	#           The 2 chosen inputs can be scalar or vector, but must have same length
	#           tol is the tolerance level for convergence
	#           maxit is the maximum number of iterations allowed
	#           Note - tolerance level, tol, is relevant for case 9 only
	# Outputs:  Matrix (or 3-way array) of life table(s) matching given inputs;
	#           plus associated values of Q5, k, e0, QQa, QQb, and Q1
	# Test that at least of one of Q1 and Q5 is null
	tmp <- c(is.null(Q5), is.null(Q1))
	if (sum(tmp)==0) { print("Error: cannot have both Q1 and Q5 as inputs"); break }
	# Test that at least of one of QQa and QQb is null
	tmp <- c(is.null(QQa), is.null(QQb))
	if (sum(tmp)==0) { print("Error: cannot have both QQa=45q15 and QQb=35q15 as inputs"); break }
	# Test that exactly two inputs are non-null
	tmp <- c(is.null(Q5), is.null(k), is.null(e0), is.null(QQa), is.null(QQb), is.null(Q1))
	if (sum(tmp)!=4) { print("Error: must have exactly two inputs"); break }
	# There are 13 cases:  "6 choose 2" = 15, but we disallow two cases (Q1 and Q5, or QQa and QQb)
	# Determine case number (see below for definitions)
	if (sum(tmp==c(F,F,T,T,T,T))==6) { case <- "1" ; tmp1 <- Q5; tmp2 <- k   }
	if (sum(tmp==c(F,T,F,T,T,T))==6) { case <- "2" ; tmp1 <- Q5; tmp2 <- e0  }
	if (sum(tmp==c(F,T,T,F,T,T))==6) { case <- "3a"; tmp1 <- Q5; tmp2 <- QQa }
	if (sum(tmp==c(F,T,T,T,F,T))==6) { case <- "3b"; tmp1 <- Q5; tmp2 <- QQb }
	if (sum(tmp==c(T,F,T,T,T,F))==6) { case <- "4" ; tmp1 <- Q1; tmp2 <- k   }
	if (sum(tmp==c(T,T,F,T,T,F))==6) { case <- "5" ; tmp1 <- Q1; tmp2 <- e0  }
	if (sum(tmp==c(T,T,T,F,T,F))==6) { case <- "6a"; tmp1 <- Q1; tmp2 <- QQa }
	if (sum(tmp==c(T,T,T,T,F,F))==6) { case <- "6b"; tmp1 <- Q1; tmp2 <- QQb }
	if (sum(tmp==c(T,F,F,T,T,T))==6) { case <- "7" ; tmp1 <- k ; tmp2 <- e0  }
	if (sum(tmp==c(T,F,T,F,T,T))==6) { case <- "8a"; tmp1 <- k ; tmp2 <- QQa }
	if (sum(tmp==c(T,F,T,T,F,T))==6) { case <- "8b"; tmp1 <- k ; tmp2 <- QQb }
	if (sum(tmp==c(T,T,F,F,T,T))==6) { case <- "9a"; tmp1 <- e0; tmp2 <- QQa }
	if (sum(tmp==c(T,T,F,T,F,T))==6) { case <- "9b"; tmp1 <- e0; tmp2 <- QQb }
	# Test that the two non-null inputs have same length
	if (length(tmp1)!=length(tmp2))	{ print("Error: the two chosen inputs must have same length"); break }
	if (length(tmp1)==1) {
		# Cases 1-3:  Q5 is known, plus k, e0, or QQ
		if (case=="1") {
			tmp <- lthat.logquad(coefs, sex, Q5, k) }
		if (case=="2") {
			tmp.fcn  <- function(k, Q5, e0, sex, coefs) lthat.logquad(coefs, sex, Q5, k)$lt.exact[1,"ex"] - e0
			tmp.root <- uniroot(tmp.fcn, c(-10,10), Q5=Q5, e0=e0, sex=sex, coefs=coefs)
			k <- tmp.root$root
			tmp <- lthat.logquad(coefs, sex, Q5, k) }
		if (case=="3a") {
			tmp.fcn  <- function(k, Q5, QQ, sex, coefs) {
				tmp <- lthat.logquad(coefs, sex, Q5, k)$lt.exact
				return(1 - tmp["60-64", "lx"] / tmp["15-19", "lx"] - QQ) }
			tmp.root <- uniroot(tmp.fcn, c(-10,10), Q5=Q5, QQ=QQa, sex=sex, coefs=coefs) }
		if (case=="3b") {
			tmp.fcn  <- function(k, Q5, QQ, sex, coefs) {
				tmp <- lthat.logquad(coefs, sex, Q5, k)$lt.exact
				return(1 - tmp["50-54", "lx"] / tmp["15-19", "lx"] - QQ) }
			tmp.root <- uniroot(tmp.fcn, c(-10,10), Q5=Q5, QQ=QQb, sex=sex, coefs=coefs) }
		if (case=="3a" | case=="3b") {
			k <- tmp.root$root
			tmp <- lthat.logquad(coefs, sex, Q5, k) }
		# Cases 4-6:  Q1 is known, plus k, e0, or QQ;
		#             after finding Q5 (assume k=0, but it doesn't matter), these become Cases 1-3
		if (case=="4" | case=="5" | case=="6a" | case=="6b") {
			tmp.fcn  <- function(Q5, Q1, sex, coefs) lthat.logquad(coefs, sex, Q5, 0)$lt.exact[1, "qx"] - Q1
			tmp.root <- uniroot(tmp.fcn, c(1e-4,0.8), Q1=Q1, sex=sex, coefs=coefs)
			Q5 <- tmp.root$root }
		if (case=="4") {
			tmp <- lthat.any2.logquad(coefs, sex, Q5=Q5, k=k) }
		if (case=="5") {
			tmp <- lthat.any2.logquad(coefs, sex, Q5=Q5, e0=e0) }
		if (case=="6a") {
			tmp <- lthat.any2.logquad(coefs, sex, Q5=Q5, QQa=QQa) }
		if (case=="6b") {
			tmp <- lthat.any2.logquad(coefs, sex, Q5=Q5, QQb=QQb) }
		# Cases 7-8:  k is known, plus e0 or QQ; must find Q5
		if (case=="7") {
			tmp.fcn  <- function(Q5, k, e0, sex, coefs) lthat.logquad(coefs, sex, Q5, k)$lt.exact[1,"ex"] - e0
			tmp.root <- uniroot(tmp.fcn, c(1e-4,0.8), k=k, e0=e0, sex=sex, coefs=coefs)
			Q5 <- tmp.root$root
			tmp <- lthat.logquad(coefs, sex, Q5, k) }
		if (case=="8b") {
			tmp.fcn  <- function(Q5, k, QQ, sex, coefs) {
				tmp <- lthat.logquad(coefs, sex, Q5, k)$lt.exact
				return(1 - tmp["60-64", "lx"] / tmp["15-19", "lx"] - QQa) }
			tmp.root <- uniroot(tmp.fcn, c(1e-4,0.8), k=k, QQ=QQa, sex=sex, coefs=coefs) }
		if (case=="8b") {
			tmp.fcn  <- function(Q5, k, QQ, sex, coefs) {
				tmp <- lthat.logquad(coefs, sex, Q5, k)$lt.exact
				return(1 - tmp["50-54", "lx"] / tmp["15-19", "lx"] - QQ) }
			tmp.root <- uniroot(tmp.fcn, c(1e-4,0.8), k=k, QQ=QQb, sex=sex, coefs=coefs) }
		if (case=="8a" | case=="8b") {
			Q5 <- tmp.root$root
			tmp <- lthat.logquad(coefs, sex, Q5, k) }
		# Case 9:     QQ and e0 are known; must find both Q5 and k
		if (case=="9a" | case=="9b") {
			k <- Q5 <- 0; iter <- crit <- 1
			while (crit>tol & iter<=maxit) {
				k.old <- k; Q5.old <- Q5
				# Get Q5 from e0 assuming k
				Q5 <- lthat.any2.logquad(coefs, sex, k=k, e0=e0)$Q5
				# Get k from QQa or QQb asuming Q5
				if (case=="9a") tmp <- lthat.any2.logquad(coefs, sex, Q5=Q5, QQa=QQa)
				if (case=="9b") tmp <- lthat.any2.logquad(coefs, sex, Q5=Q5, QQb=QQb)
				k <- tmp$k; lt <- tmp$lt; lt.exact <- tmp$lt.exact
				crit <- sum(abs(c(k, Q5) - c(k.old, Q5.old)))
				# Case 9 does not always converge.  If there are problems,
				# un-comment the following 2 lines to trace what is happening
				# print(c("    iter         Q5           k          e0          QQ         crit         tol"))
				# print(c(iter, Q5, k, tmp$e0, if (case=="9a") tmp$QQa else if (case=="9b") tmp$QQb, crit, tol))
				iter <- iter + 1 }
				if (iter>maxit) print("Warning: number of iterations reached maximum without convergence") }
		# Extract lt, lt.exact, Q5, and k from tmp
		lt <- tmp$lt; lt.exact <- tmp$lt.exact; Q5 <- tmp$Q5; k <- tmp$k
		# Extract e0, QQa, QQb, and Q1 from final (exact) life table
		e0  <- lt.exact["0", "ex"]
		QQa <- 1 - lt.exact["60-64", "lx"] / lt.exact["15-19", "lx"]
		QQb <- 1 - lt.exact["50-54", "lx"] / lt.exact["15-19", "lx"]
		Q1  <- lt.exact["0", "qx"]
		mx1x1 <- mxfun(coefs, sex, 0.5:110.5, Q5, k); names(mx1x1) <- 0:110
		# Return life table plus values of the 6 possible inputs
		return(list(lt=lt, lt.exact=lt.exact, Q5=Q5, k=k, e0=e0, QQa=QQa, QQb=QQb, Q1=Q1, mx1x1=mx1x1)) }
	else {
		ages     <- c("0", "1-4", paste(seq(5,105,5),seq(9,109,5),sep="-"), "110+")
		ages.1x1 <- 0:110
		lt.cols <- c("mx","qx","ax","lx","dx","Lx","Tx","ex")
		lt <- lt.exact <- array(NA, dim=c(length(ages), length(lt.cols), length(tmp1)), dimnames=list(ages, lt.cols, names(tmp1)))
		mx1x1 <- array(NA, dim=c(length(ages.1x1), length(tmp1)), dimnames=list(ages.1x1, names(tmp1)))
		if (case=="1") {
			e0 <- QQa <- QQb <- Q1 <- tmp1
			for (j in 1:length(tmp1)) {
				tmp <- lthat.any2.logquad(coefs, sex, Q5=Q5[j], k=k[j])
				lt[,,j] <- tmp$lt; lt.exact[,,j] <- tmp$lt.exact; mx1x1[,j] <- tmp$mx1x1
				e0[j] <- tmp$e0; QQa[j] <- tmp$QQa; QQb[j] <- tmp$QQb; Q1[j] <- tmp$Q1 } }
		if (case=="2") {
			k  <- QQa <- QQb <- Q1 <- tmp1
			for (j in 1:length(tmp1)) {
				tmp <- lthat.any2.logquad(coefs, sex, Q5=Q5[j], e0=e0[j])
				lt[,,j] <- tmp$lt; lt.exact[,,j] <- tmp$lt.exact; mx1x1[,j] <- tmp$mx1x1
				k[j] <- tmp$k; QQa[j] <- tmp$QQa; QQb[j] <- tmp$QQb; Q1[j] <- tmp$Q1 } }
		if (case=="3a") {
			k  <- e0 <- QQb <- Q1 <- tmp1
			for (j in 1:length(tmp1)) {
				tmp <- lthat.any2.logquad(coefs, sex, Q5=Q5[j], QQa=QQa[j])
				lt[,,j] <- tmp$lt; lt.exact[,,j] <- tmp$lt.exact; mx1x1[,j] <- tmp$mx1x1
				k[j] <- tmp$k; e0[j] <- tmp$e0; QQb[j] <- tmp$QQb; Q1[j] <- tmp$Q1 } }
		if (case=="3b") {
			k  <- e0 <- QQa <- Q1 <- tmp1
			for (j in 1:length(tmp1)) {
				tmp <- lthat.any2.logquad(coefs, sex, Q5=Q5[j], QQb=QQb[j])
				lt[,,j] <- tmp$lt; lt.exact[,,j] <- tmp$lt.exact; mx1x1[,j] <- tmp$mx1x1
				k[j] <- tmp$k; e0[j] <- tmp$e0; QQa[j] <- tmp$QQa; Q1[j] <- tmp$Q1 } }
		if (case=="4") {
			e0 <- QQa <- QQb <- Q5 <- tmp1
			for (j in 1:length(tmp1)) {
				tmp <- lthat.any2.logquad(coefs, sex, Q1=Q1[j], k=k[j])
				lt[,,j] <- tmp$lt; lt.exact[,,j] <- tmp$lt.exact; mx1x1[,j] <- tmp$mx1x1
				e0[j] <- tmp$e0; QQa[j] <- tmp$QQa; QQb[j] <- tmp$QQb; Q5[j] <- tmp$Q5 } }
		if (case=="5") {
			k  <- QQa <- QQb <- Q5 <- tmp1
			for (j in 1:length(tmp1)) {
				tmp <- lthat.any2.logquad(coefs, sex, Q1=Q1[j], e0=e0[j])
				lt[,,j] <- tmp$lt; lt.exact[,,j] <- tmp$lt.exact; mx1x1[,j] <- tmp$mx1x1
				k[j] <- tmp$k; QQa[j] <- tmp$QQa; QQb[j] <- tmp$QQb; Q5[j] <- tmp$Q5 } }
		if (case=="6a") {
			k  <- e0 <- Q5 <- QQb <- tmp1
			for (j in 1:length(tmp1)) {
				tmp <- lthat.any2.logquad(coefs, sex, Q1=Q1[j], QQa=QQa[j])
				lt[,,j] <- tmp$lt; lt.exact[,,j] <- tmp$lt.exact; mx1x1[,j] <- tmp$mx1x1
				k[j] <- tmp$k; e0[j] <- tmp$e0; Q5[j] <- tmp$Q5; QQb[j] <- tmp$QQb } }
		if (case=="6b") {
			k  <- e0 <- Q5 <- QQa <- tmp1
			for (j in 1:length(tmp1)) {
				tmp <- lthat.any2.logquad(coefs, sex, Q1=Q1[j], QQb=QQb[j])
				lt[,,j] <- tmp$lt; lt.exact[,,j] <- tmp$lt.exact; mx1x1[,j] <- tmp$mx1x1
				k[j] <- tmp$k; e0[j] <- tmp$e0; Q5[j] <- tmp$Q5; QQa[j] <- tmp$QQa } }
		if (case=="7") {
			Q5 <- QQa <- QQb <- Q1 <- tmp1
			for (j in 1:length(tmp1)) {
				tmp <- lthat.any2.logquad(coefs, sex, k=k[j], e0=e0[j])
				lt[,,j] <- tmp$lt; lt.exact[,,j] <- tmp$lt.exact; mx1x1[,j] <- tmp$mx1x1
				Q5[j] <- tmp$Q5; QQa[j] <- tmp$QQa; QQb[j] <- tmp$QQb; Q1[j] <- tmp$Q1 } }
		if (case=="8a") {
			Q5 <- e0 <- QQb <- Q1 <- tmp1
			for (j in 1:length(tmp1)) {
				tmp <- lthat.any2.logquad(coefs, sex, k=k[j], QQa=QQa[j])
				lt[,,j] <- tmp$lt; lt.exact[,,j] <- tmp$lt.exact; mx1x1[,j] <- tmp$mx1x1
				Q5[j] <- tmp$Q5; e0[j] <- tmp$e0; QQb[j] <- tmp$QQb; Q1[j] <- tmp$Q1 } }
		if (case=="8b") {
			Q5 <- e0 <- QQa <- Q1 <- tmp1
			for (j in 1:length(tmp1)) {
				tmp <- lthat.any2.logquad(coefs, sex, k=k[j], QQb=QQb[j])
				lt[,,j] <- tmp$lt; lt.exact[,,j] <- tmp$lt.exact; mx1x1[,j] <- tmp$mx1x1
				Q5[j] <- tmp$Q5; e0[j] <- tmp$e0; QQa[j] <- tmp$QQa; Q1[j] <- tmp$Q1 } }
		if (case=="9a") {
			Q5 <- k  <- QQb <- Q1 <- tmp1
			for (j in 1:length(tmp1)) {
				tmp <- lthat.any2.logquad(coefs, sex, e0=e0[j], QQa=QQa[j], tol=tol, maxit=maxit)
				lt[,,j] <- tmp$lt; lt.exact[,,j] <- tmp$lt.exact; mx1x1[,j] <- tmp$mx1x1
				Q5[j] <- tmp$Q5; k[j] <- tmp$k; QQb[j] <- tmp$QQb; Q1[j] <- tmp$Q1 } }
		if (case=="9b") {
			Q5 <- k  <- QQa <- Q1 <- tmp1
			for (j in 1:length(tmp1)) {
				tmp <- lthat.any2.logquad(coefs, sex, e0=e0[j], QQb=QQb[j], tol=tol, maxit=maxit)
				lt[,,j] <- tmp$lt; lt.exact[,,j] <- tmp$lt.exact; mx1x1[,j] <- tmp$mx1x1
				Q5[j] <- tmp$Q5; k[j] <- tmp$k; QQa[j] <- tmp$QQa; Q1[j] <- tmp$Q1 } }
		return(list(lt=lt, lt.exact=lt.exact, Q5=Q5, k=k, e0=e0, QQa=QQa, QQb=QQb, Q1=Q1, mx1x1=mx1x1)) } }

mxfun <- function(coefs, sex, x, Q5, k=0) {
	# Continuous mux function using spline interpolation
	# Returns mx of log-quadratic model given Q5 and k
	tmp <- lthat.logquad(coefs, sex, Q5, k)
	log.mx5 <- log(tmp$lt.exact[, "mx"])
	tmp1 <- spline(c(0.5,3,7.5), log.mx5[1:3], xout=x)$y
	tmp2 <- spline(c(3,seq(7.5,112.5,5)), log.mx5[-1], xout=x)$y
	# Create weights that fall rapidly from 1 to 0 around ages 0-3
	w <- 1 - plogis(x, location=1.5, scale=0.4)
	log.mx <- w*tmp1 + (1-w)*tmp2
	# Correct cross-overs at youngest ages for non-zero values of k (force to equal mx for k=0)
	if (k!=0) {
		tmp0 <- log(mxfun(coefs, sex, x, Q5, k=0))
		log.mx <- if (k>0) ifelse(tmp0>log.mx, tmp0, log.mx) else ifelse(tmp0<log.mx, tmp0, log.mx) }
		mx <- exp(log.mx)
		if (length(mx)>1) names(mx) <- x
		# Force rate for age 0 to equal original value
		mx[x==0.5] <- exp(log.mx5[1])
		return(mx) }

coale.demeny.a0 <- function(m0,sex) {
	if (sum(m0<=0)>0) { print("error: m0 must be greater than 0"); break }
	if      (sex=="Male"  ) a0 <- ifelse(m0>=0.107,0.330,0.045+2.684*m0)
	else if (sex=="Female") a0 <- ifelse(m0>=0.107,0.350,0.053+2.800*m0)
	else if (sex=="Total" ) a0 <- 0.5*(coale.demeny.a0(m0,sex="Male")+coale.demeny.a0(m0,sex="Female"))
	else                    { print("error: 'sex' must be 'Male', 'Female', or 'Total'"); break }
	return(a0) }

coale.demeny.4a1 <- function(m0,sex) {
	if (sum(m0<=0)>0) { print("error: m0 must be greater than 0"); break }
	if      (sex=="Male"  ) a1 <- ifelse(m0>=0.107,1.352,1.651-2.816*m0)
	else if (sex=="Female") a1 <- ifelse(m0>=0.107,1.361,1.522-1.518*m0)
	else if (sex=="Total" ) a1 <- 0.5*(coale.demeny.4a1(m0,sex="Male")+coale.demeny.4a1(m0,sex="Female"))
	else                    { print("error: 'sex' must be 'Male', 'Female', or 'Total'"); break }
	return(a1) }

lt.from.mx <- function(mx, sex, Q5=NULL) {
	# Input:
	#   vector or matrix of death rates, mx
	#   sex ("Female", "Male", or "Total")
	#   scalar or vector of Q5 values (can be null) 
	# Outputs:
	#   lt       is a matrix or array with one or more life tables (rounded)
	#   lt.exact is a matrix or array with one or more life tables (exact values)
	# Assumes age groups:  0, 1-4, 5-9, ..., w+ (labeled as such)
	if (is.vector(mx)) {
		ages <- names(mx)
		N <- length(mx)
		# Extract lower bound of open age interval, w, in numeric format
		# This requires testing whether w is 2- or 3-digit
		tmp <- names(mx)[N]
		w <- if (substr(tmp,1,1)==1) as.numeric(substr(tmp,1,3)) else as.numeric(substr(tmp,1,2))
		x <- c(0,1,seq(5,w,5))
		nx <- c(diff(x),Inf)
		qx <- 1-exp(-nx*mx)
		ax <- nx + 1/mx - nx/qx 
		# Below age 5
		ax[1] <- coale.demeny.a0( mx[1],sex)
		ax[2] <- coale.demeny.4a1(mx[1],sex)
		qx[1] <-                  nx[1]*mx[1] / (1 + (nx[1]-ax[1])*mx[1])
		# If Q5 is given, 4q1 is derived from 1q0 and 5q0 and 4m1 is re-computed; otherwise, 4q1 is derived directly from 4m1
		if (is.null(Q5))
			qx[2] <- nx[2]*mx[2] / (1 + (nx[2]-ax[2])*mx[2])
		else {
			qx[2] <- 1 - (1-Q5)/(1-qx[1])
			mx[2] <- qx[2] / ( 4 - (4-ax[2])*qx[2] ) }
		# Open age interval
		qx[N] <- 1
		ax[N] <- 1/mx[N]
		# Make life table
		lx <- 100000 * c(1,cumprod(1-qx)[1:(N-1)])
		dx <- qx * lx
		Lx <- nx*lx - (nx-ax)*dx; Lx[N] <- dx[N]/mx[N]
		Tx <- rev(cumsum(rev(Lx)))
		ex <- Tx/lx
		lt <- cbind(round(mx,6), round(qx,6), round(ax,2), round(lx), round(dx), round(Lx), round(Tx), round(ex,2))
		lt.exact <- cbind(mx, qx, ax, lx, dx, Lx, Tx, ex)
		dimnames(lt) <- dimnames(lt.exact) <- list(ages, c("mx","qx","ax","lx","dx","Lx","Tx","ex")) }
	else if (is.matrix(mx)) {
		lt <- lt.exact <- array(NA,dim=c(nrow(mx), 8, ncol(mx)),
					dimnames=list(dimnames(mx)[[1]], c("mx","qx","ax","lx","dx","Lx","Tx","ex"), dimnames(mx)[[2]]))
		for (j in 1:ncol(mx)) {
			tmp <- lt.from.mx(mx[,j], sex, Q5=if (is.null(Q5)) NULL else Q5[j])
			lt[,,j] <- tmp$lt; lt.exact[,,j] <- tmp$lt.exact } }
	return(list(lt=lt, lt.exact=lt.exact)) }

plot.mx <- function(x, y, legend.txt, text.txt) {
	cex.axis <- 1.8
	cex.lab <- 1.8
	cex.txt <- 2.2
	cex.main <- 2.2
	cex.pt <- 1.5
	cex.legend <- 1.5
	par(mar=c(4.5,4.5,1,1), cex=1, mgp=c(2.8,1,0), bg=3)
	matplot(x, y,
		xlab="Age (in years)", ylab="Death rate (log scale)",
		type="l", lty=1, col=2, col.lab=1, lwd=seq(1,3,length=ncol(y)), log="y", axes=F, cex=cex.pt, cex.lab=cex.lab)
	axis(1, at=seq(0,110,10), cex.axis=cex.axis, col.axis=1, col.ticks=1, col=3)
	axis(2, at=c(0.0005,0.005,0.05,0.5), labels=c("0.0005","0.005","0.05","0.5"),
		cex.axis=cex.axis, col.axis=1, col.ticks=1, col=3)
	box(col=1)
	legend(110, min(y), legend.txt,
		xjust=1, yjust=0, lty=1, col=2, text.col=1, lwd=seq(1,3,length=ncol(y)), cex=cex.legend, bty="n", y.intersp=1.2)
	text(3, max(y), text.txt, adj=c(0,1), cex=cex.txt, col=1) }

