library(Rcpp)
library(igraph)

sourceCpp("/src/edmondsKarp.cpp") 
capMatrix <- matrix(
  c(0, 8, 0, 0, 3, 0,
    0, 0, 9, 0, 0, 0,
    0, 0, 0, 0, 7, 2,
    0, 0, 0, 0, 0, 5,
    0, 0, 7, 4, 0, 0,
    0, 0, 0, 0, 0, 0),
  byrow = TRUE, nrow = 6)

# Create an igraph graph object from the capacity matrix
g <- graph_from_adjacency_matrix(capMatrix, mode = "directed", weighted = TRUE)

# Circular layout
layout_circle <- layout_in_circle(g)
# Reorder the layout to start from S and move clockwise
layout_ordered <- layout_circle[c(1,2,3,6,5,4), ]


# Run the Edmonds-Karp algorithm
source_node <- 1
sink_node <- 6
flowMatrix <- edmondsKarp(capMatrix, source_node - 1, sink_node - 1) # Adjust indexing for Rcpp

# Annotate edges with flow/capacity
edge_labels <- apply(get.edgelist(g), 1, function(e) {
  u <- e[1]
  v <- e[2]
  paste0(flowMatrix[u, v], "/", capMatrix[u, v])
})




windows()
plot(g,
     layout = layout_ordered,
     edge.label = E(g)$weight, 
     edge.arrow.size = 0.5,
     vertex.color = "lightblue",
     vertex.size = 30,
     vertex.label.cex = 1.5,
     edge.label = edge_labels,
     edge.curved = 0.2,
     main = "Graphe Initial")


# Flots maximums
windows()
plot(g, 
     layout = layout_ordered, 
     edge.arrow.size = 0.5,
     vertex.color = "lightblue",
     vertex.size = 30,
     vertex.label.cex = 1.5,
     edge.label = edge_labels,
     edge.curved = 0.2,
     main = "Graphe avec Flots Maximums"
)


