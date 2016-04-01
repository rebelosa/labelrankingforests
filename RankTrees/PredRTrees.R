# PredRTrees.R
# 
# PREDICT RANKING TREES
#
# Author: claudio rebelo sa

###############################################################################


# Main function
PredRTrees <- function (modelo, dados.prev) {
	#
	dados.prev <- data.frame(dados.prev)
	if (nrow(dados.prev) > 1) dados.prev <- newMatrix(dados.prev)
	#
	
  rankings <- t(apply(dados.prev, 1, function(row) {
    avaliar.modelo(modelo, row)
  }))
	
	rownames(rankings) <- c(rownames(dados.prev))
	
	rankings
}


parte.arvore <- function(arvore, side) {
	
  indice <- which(arvore[,"Par"] == rownames(arvore[1,]))
	
	if (side){
	  arvore[(indice[1]):(indice[2] - 1),]
	} else {
	  arvore[(indice[2]):nrow(arvore), ]
	}
}

avaliar.modelo <- function(arvore, row) {
  
  valor.atrib = row[arvore[1,"Atrib"]]
  nivel = arvore[1,"nivel"]
  
  if (arvore[1,"Pos"] == "LEAF") {
    return( as.numeric(arvore[1,8:ncol(arvore)]) )
  }
  else {
    val <- as.numeric(arvore[1,"Val"])
  }

  if(nivel == "< ") {
    
   if(valor.atrib < val) {
      arvore <- parte.arvore(arvore, TRUE)
    } else {
      arvore <- parte.arvore(arvore, FALSE)
    }
    r <- avaliar.modelo(arvore, row)
    return(r)
  } 
}	

newMatrix <- function(dados) {
	
	for (i in 1:ncol(dados)) {
		q <- apply(dados[i],2,function(col) {ifelse(is.na(col),0,col)})
		if (is.numeric(q)) {
			media <- mean(dados[,i][which(dados[i] != "NA")])	
			qq <- which(sapply(dados[,i],function(col){if (is.na(col)) col}) == "NA")
			dados[i] <- replace(dados[,i], qq, media)
		}
		else {	
			ord <- sort(dados[,i])
			xt <- tabulate(dados[,i][which(dados[i] != "NA")])
			xmode <- which(xt == max(xt))
			moda <- levels(as.factor(ord))[xmode]
			qq <- which(sapply(dados[,i],function(col){if (is.na(col)) factor(col)}) == "NA")
			dados[i] <- replace(dados[,i], qq, moda)
			
		}	
	}
	return(dados)
}
