# Plot RankTrees
R code to visualize RankTrees

source('RK_graph.R')

t <- rankTrees(data,targets_matrix)  #qualquer modelo que tenhas criado

z <- createTree(getExtendedTree(t))

  plot(z,layout = layout_as_tree,
       edge.width = 2,
       edge.arrow.width = 1,
       vertex.size = (strwidth(V(z)$label) + strwidth("oooo")) * 100,
       vertex.size2 = strheight("I") * 2 * 100,
       edge.arrow.size = 1,
       vertex.size2 = 3,
       vertex.label.cex = 1,
       asp = 0.35,
       vertex.label.cex = 0.4, vertex.label.degree = -pi/2,
       margin = -0.3)
