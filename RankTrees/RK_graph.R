library(igraph)

cleanPairsNodes <- function(level_nodes){
  pos <- level_nodes$Pos
  
  if(length(grep(' T',pos, value=TRUE)) > 0 & length(grep(' F',pos, value=TRUE)) > 0){
    level_nodes <- level_nodes
    return(level_nodes)
  }  
  
  if(length(grep(' T',pos, value=TRUE)) > 0 & length(grep(' F',pos, value=TRUE)) == 0){
    val <- grep(' T',pos, value=TRUE)
    indexes <- which(level_nodes$Pos != val)
    
    level_nodes[indexes,]$Pos <- paste0(level_nodes[indexes,]$Pos, " F")
    return(level_nodes)
  }
  
  if(length(grep(' T',pos, value=TRUE)) == 0 & length(grep(' F',pos, value=TRUE)) > 0){
    val <- grep(' F',pos, value=TRUE)
    indexes <- which(level_nodes$Pos != val)
    
    level_nodes[indexes,]$Pos <- paste0(level_nodes[indexes,]$Pos, " T")
    return(level_nodes)
  }
  
  level_nodes[1,]$Pos <- paste0(level_nodes[1,]$Pos, " T")
  level_nodes[2,]$Pos <- paste0(level_nodes[2,]$Pos, " F")
  return(level_nodes)
}

getExtendedTree <- function(tree1){
  keepcolumns <- c("Par","Pos","Level")
  new_tree <- tree1[,which(colnames(tree1) %in% keepcolumns)]
  new_tree$conditions <- apply(tree1[,4:6],1,function(x){paste0(x,collapse = '')})
  new_tree$target <- apply(tree1[,8:dim(tree1)[2]],1,function(x){paste0(x,collapse = ',')})
  
  corrected_tree <- NULL
  
  for(index in 0:max(new_tree$Level)){
    
    tmp <- new_tree[which(new_tree$Level == index),]
    
    if(dim(tmp)[1] != 1){
      if(dim(tmp)[1] > 2){
        
        parents <- unique(tmp$Par)
        c_nodes <- NULL
        
        for(elem in parents){
          new_tmp <- tmp[which(tmp$Par == elem),]
          res <- cleanPairsNodes(new_tmp)
          c_nodes <- rbind(c_nodes,res)
        }
      }
      else {
        c_nodes <- cleanPairsNodes(tmp)
      }
      corrected_tree <- rbind(corrected_tree,c_nodes)
    }
    else {
      corrected_tree <- rbind(corrected_tree,tmp)  #root
    }
  }
  
  corrected_tree <- corrected_tree[ order(as.numeric(row.names(corrected_tree))), ]
  corrected_tree
}


createTree <- function(d){
  
  g <- make_empty_graph() 
  
  for(index in 1:dim(d)[1]){
    node <- d[index,]
    
    if(length(grep('LEAF',node$Pos, value=TRUE)) > 0){
      new_node <- vertex(index, label = node$target, shape="rectangle", color = 	"white")
    }
    else {
      new_node <- vertex(index, label = node$conditions, shape="rectangle", color = "#8ccaeb")
    }
    g <- g + new_node
  }
  
  for(index in 1:dim(d)[1]){
    node <- d[index,]
    
    if(node$Par != "0"){
      new_edge <- edge(node$Par,index)
      
      if(length(grep(' T',node$Pos, value=TRUE)) > 0){
        new_edge <- edge(node$Par,index,  color = "#6B8E23")
      }
      if(length(grep(' F',node$Pos, value=TRUE)) > 0){
        new_edge <- edge(node$Par,index,  color = "red")
      }
      g <- g + new_edge
    }
  }
  g
}
