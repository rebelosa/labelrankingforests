# TODO: Add comment
# 
# Author: claudio
###############################################################################

source("./RankTrees/RankTrees.R")
source("./RankTrees/PredRTrees.R")
require("parallel")

pred <- function( x, y, k = 1:10, seed = 10, gama = 0.98, splitCrit = "CS")
{
  set.seed(seed)
  
  # k[1]=0 leave-one-out; k[]>0 cross validation
  if (k[1]==0) {
    S <- generate.cv(nrow(x),nrow(x))
  } else {
    S <- generate.cv(nrow(x),10)
  }
  
  frst <- mcmapply(function(i) {
    randfres <- NULL
    
	s <- S[i,]
		
	d <- x[-s,,drop=FALSE]
	xs <- x[s,,drop=FALSE]
	
	res <- 0
	dx <- x[-s,,drop=FALSE]
	dy <- y[-s,,drop=FALSE]
	depthTree <- 0
	sizeForest <- 100
	for (f in 1:sizeForest) {
		set.seed(f)
		subset <- sample(1:nrow(dx),nrow(dx),replace=T)
		    
  		model <- rankTrees(dx[subset,,drop=FALSE], dy[subset,,drop=FALSE], gama = gama, crit = splitCrit)
  		  
  		depthTree <- depthTree + max(as.numeric(model[,"Level"]))
  		  
  		# Prediction
  		res <- res + PredRTrees(model, x[s,,drop=FALSE])
	}
		  
	randfres <- mean(
		    sapply(1:nrow(res), function(r)
		    {
		      cor(res[r,], y[s,,drop=FALSE][r,], method="kendall")
		    })
		  )
		  
	depthTree <- round(depthTree/sizeForest, 1)
	randfres <- c(tau = randfres, depth = depthTree)
	}, k,
	mc.silent = FALSE,
	mc.cores = ifelse(Sys.info()['sysname']=="Windows",1,detectCores())
	)
	
	print(rowMeans(frst , na.rm = T))
	mean.rst <- round(rowMeans(frst	, na.rm = T),3)
	sd.rst <- round(apply(frst,	1, sd, na.rm = T),3)
	
	frst
}
#pred("iris",disc="m11",mconf=90, mimp=0.01, Xmx = "1000M")

generate.cv=function (n, m) {
	v <- sample(n,n)
	
	j <- (floor(n/m)*m)
	
	cbind( matrix(v[0:j], m, ), if (j<n){ c( v[(j+1):n] , rep.int(0,m-(n-j)) ) } )
}