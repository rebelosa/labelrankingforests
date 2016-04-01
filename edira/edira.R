# This version is based on the package http://www.inside-r.org/packages/cran/discretization/docs/mdlp
#
# version 1.0
# Author: claudio rebelo sa
###############################################################################

require("parallel")
#x - Matrix with independent variables
#y - Matrix with Rankings
#method - Similarity method ("kendall" or "spearman")
mdlp.rank <- function (x, y, method = "kendall") 
{
  p <- ncol(x)
  cutp <- list()
  
  y <- trans.ranking(y, method)
  
  cutp <- mcmapply(function(i) {
    x <- x[, i, drop=FALSE]
    sort(cutPoints.rank(x, y))
  }, 1:p,
  SIMPLIFY = FALSE,
  mc.cores = ifelse(Sys.info()['sysname']=="Windows",1,detectCores())
  )
  
  xd <- sapply(1:ncol(x), function(j){
    findInterval(x[,j], c(-Inf, cutp[[j]], Inf))
  })
  
  colnames(xd) <- colnames(x)
  
  return(list(cutp = cutp, Disc.data = xd))
}

trans.ranking <- function(y, method = "kendall")
{
	y <- t(round(cor(t(unique(y)), t(y), method = method), 7))
	y <- (y+1)/2
	y
}

#entropy of the set of rankings y
ent.rank <- function (y)
{
	if (nrow(y) == 1)
	{
		0
	} else
	{	
		M <- apply(y, 2, max)
		y <- y[,M==1,drop=FALSE]
		
		p <- colSums(y==1)/nrow(y)
		
		e <- sum(p * mylog(p) )*mylog(mean(y))
		return(e)
	}
}

cutIndex.rank <- function (x, y) 
{
	n <- nrow(y)
	initEnt <- Inf
	entropy <- initEnt
	ci <- NULL
	for (i in 1:(n - 1)) {
    if ( !is.na(x[i+1]) & !is.na(x[i]) ) {
  		if (x[i + 1] != x[i]) {
  			ct <- (x[i] + x[i + 1])/2
  			wx <- which(x <= ct)
  			wn <- length(wx)/n
  			#!
  			e1 <- wn * ent.rank(y[wx,,drop=FALSE]) 
  			#!
  			e2 <- (1 - wn) * ent.rank(y[-wx,,drop=FALSE])
  			val <- round(e1 + e2, 7)
  			
  			if (val < entropy) {
  				entropy <- val
  				ci <- i
  			}
  		}
    }
	}
	if (is.null(ci)) 
		return(NULL)
	return(c(ci, entropy))
}

cutPoints.rank <- function (x, y) 
{
	od <- order(x)
	xo <- x[od]
	yo <- y[od,,drop=FALSE]
	depth <- 1
	gr <- function(low, upp, depth = depth) {
		x <- xo[low:upp]
		y <- yo[low:upp,,drop=FALSE]
		n <- nrow(y)
		ct <- cutIndex.rank(x, y)
		if (is.null(ct)) 
			return(NULL)
		ci <- ct[1]
		entropy <- ct[2]
		ret <- mdlStop.rank(ci, y, entropy)
		if (is.null(ret)) 
			return(NULL)
		return(c(ci, depth + 1))
	}
	part <- function(low = 1, upp = length(xo), cutPoints = NULL, depth = depth)
	{
		x <- xo[low:upp]
		##
		y <- yo[low:upp,,drop=FALSE]
		##
		n <- length(x)
		if (n < 2) 
			return(cutPoints)
		cc <- gr(low, upp, depth = depth)
		ci <- cc[1]
		
		depth <- cc[2]
		if (is.null(ci)) 
			return(cutPoints)
		cutPoints <- c(cutPoints, low + ci - 1)
		cutPoints <- as.integer(sort(cutPoints))
		return(c(part(low, low + ci - 1, cutPoints, depth = depth), 
						part(low + ci, upp, cutPoints, depth = depth)))
	}
	res <- part(depth = depth)
	ci <- NULL
	cv <- numeric()
	if (!is.null(res)) {
		ci <- as.integer(res)
		cv <- (xo[ci] + xo[ci + 1])/2
	}
	res <- unique(cv)
	return(res)
}


mdlStop.rank <- function (ci, y, entropy) 
{
	n <- nrow(y)
	es <- round(ent.rank(y), 7)
	left <- 1:ci
	right <- (ci + 1):n
	gain <- round(es - entropy, 7)
	
	k <- nrow(unique(y))
	k1 <- nrow(unique(y[left,,drop=FALSE]))
	k2 <- nrow(unique(y[right,,drop=FALSE]))
	
	if (k<100){
	  delta <- mylog(3^k - 2) - (k*es - k1*ent.rank(y[left,,drop=FALSE]) - k2*ent.rank(y[right,,drop=FALSE]) )
	} else {
	  delta <- k*mylog(3) - (k*es - k1*ent.rank(y[left,,drop=FALSE]) - k2*ent.rank(y[right,,drop=FALSE]) )
	}
	
	delta <- round(delta, 7)
	cond <- mylog(n - 1)/n + delta/n
	
  if (gain <= cond)
		return(NULL)
	return(gain)
}

mylog <- function (x) 
{
	x[which(x <= 0.0000000001)] <- 1
	return(log(x))
}
