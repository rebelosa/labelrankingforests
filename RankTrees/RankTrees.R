# 
# RANKING TREES
#
# Author: claudio rebelo sa

# x - Matrix with independent variables
# y - Matrix with Rankings
# Splitting criterion (crit):
# - "CS" (requires parameter gama)
# - - gama is a parameter between [0,1]
# - "ent" 

###############################################################################


# Main function
rankTrees <- function(x, y, gama = 0.98, crit = "CS") {

	myMatrix <- matrix(,0,ncol(y) + 7)
	if (crit == "ent") source("../edira/edira.R")
	
	
	tabela.final <- data.frame( labelrankingtrees(myMatrix, "Root", x, y, gama, crit, 0, level = 0) , stringsAsFactors = FALSE)
	names(tabela.final) <- c("Par", "Pos", "Level", "Atrib", "nivel", "Val", "n", paste("L", 1:ncol(y), sep=""))
	return(tabela.final)	
}

labelrankingtrees <- function(matriz.anterior, tipo.desc, x, y, gama, crit, parent, level) {
	if (nrow(x) == 1) {
		final.results (matriz.anterior, "LEAF", " ", " ", NA, y, parent, level)	
	}
	else {	 
		rdx <- 1:ncol(x)
		s <- split(matriz.anterior, tipo.desc, x[,rdx,drop=FALSE], y, gama, crit, parent, level)
		
		if (s$leaf == FALSE) {
			
		  parent <- nrow(s$newMatrix)
		  level <- level + 1
		  matriz <- labelrankingtrees(s$newMatrix, "DESC T", x[s$idx,,drop=FALSE], y[s$idx,,drop=FALSE], gama, crit, parent = parent, level = level)
			labelrankingtrees(matriz, "DESC F", x[!s$idx,,drop=FALSE], y[!s$idx,,drop=FALSE], gama, crit, parent = parent, level = level) 
		}
		else {
			s$newMatrix
		}
	}
}

split <- function(matriz, tipo.desc, x, y, gama, crit, parent, level) {
	
	q <- apply(x, 2, rank.split, y, crit, gama)
	
	col.with.max <- colnames(x)[which.max(sapply(q, "[[", "new"))]
	
	q <- q[[col.with.max]]
	
	if (q$leaf) {
	  newMatrix <- final.results(matriz, "LEAF", " ", " ", NA, y, parent, level)	
	} else {
		newMatrix <- final.results(matriz, tipo.desc, col.with.max, q$M, q$val, y, parent, level)
	}
	list(newMatrix = newMatrix, leaf = q$leaf, idx = q$idx)
}

final.results <- function(matrix.anterior, desc, atrib, nivel, val, y, parent, level) {
  if (is.matrix(y))
  {
    mean.rank <- rank(colMeans(y))
  } else {
    mean.rank <- y
  }
	rbind( matrix.anterior, c( parent, desc, level, atrib, nivel, val, nrow(y), mean.rank) )
}


rank.split <- function(x, ys, crit, gama){
	
	idx <- order(x)
	ys <- ys[idx,]
	x.col <- x[idx]
	
	if (crit == "CS") {
  	
  	c <- cor(t(ys), method = "kendall")
  	
  	n <- nrow(c)-1
  	
  	
  	x1 <- slidem(c, FALSE)
  	x2 <- slidem(c, TRUE)
  	q.D <- ( x1*(1:n)+x2*(n:1) ) / nrow(c)
  	
  	Sparent <- (sum(c)-sum(diag(c)))/(ncol(c)*nrow(c)-length(diag(c)))
  	
    aux <- gama*(1+q.D) > ( 1+Sparent )
  	#This line looks for steps in the x values
  	aux <- aux & sapply(1:n, function(i) x.col[i]!=x.col[i+1])
  	
  	#This step avoids creating partitions with less than 3 elements
  	aux[c(1,n)] <- FALSE
  	
  	aux <- which(aux)
  	
  	if (length(aux)>0) {
  		
   		m <- aux[which.max(q.D[aux])]
   		
  		mn <- mean(c(x.col[m], x.col[m+1]))

      which.x <- x < mn
  	
  		M <- "< "
  		val = round(mn, 3)
  		leaf <- FALSE
  		newm <- q.D[m]
  		
  	} else {
  	  leaf <- TRUE
  	}
	} else if (crit == "ent") {
	  
	  yc <- trans.ranking(ys)
	  
	  ct <- cutIndex.rank(x.col, yc)
	  if (is.null(ct)){
	    ret <- NULL
	  } else {
	    m <- ct[1]
	    entropy <- ct[2]
	    ret <- mdlStop.rank(m, yc, entropy)
	  }
	  
	  if (is.null(ret)){
	    leaf <- TRUE
	  } else {
	    
	    mn <- mean(c(x.col[m], x.col[m+1]))
	    
	    which.x <- x < mn
	    
	    M <- "< "
	    val = round(mn, 3)
	    leaf <- FALSE
	    newm <- ret
	    
	  }
	} else {
	  stop("Splitting criterion not known!")
	}
	
	if (leaf) {
	  which.x <- NULL
	  mn <- NULL
	  M <- NULL
	  val <- NULL
	  m <- 1
	  newm <- -Inf
	}
	
	list(
			mn = mn,
			idx = which.x,
			M = M,
			val = val,
			leaf = leaf,
			new = newm
	)
}

slidem <- function(matrix, reverse = FALSE){
  n <- nrow(matrix)-1
  v <- (1:n)^2
  if (reverse == FALSE){
    start <- 1
    end <- ncol(matrix)-1
  } else {
    start <- ncol(matrix)
    end <- 2
  }
  
   res <- cumsum(as.numeric(sapply(start:end, function(i){
            sum(c(matrix[i,start:i], matrix[start:i,i]))-1
           })))/v
  
  if (reverse == TRUE){
    res <- res[n:1]
  }
  res
}
