#############################################
#
# Script qui contient plusieurs algorithmes de générations de 
# graphs/network suivant plusieurs théories.
#
############################################


#' Generate a random directed graph with adjacency matrix representation
#'
#' This function generates a random directed graph with adjacency matrix representation 
#' based on the Erdős-Rényi model. The graph is generated with a specified number of nodes 
#' and edges, and each edge is assigned a random capacity from a given range.
#'
#' @param num_nodes The number of nodes in the graph.
#' @param num_edges The number of edges in the graph.
#' @param capacity_range A numeric vector specifying the range of capacities for the edges.
#' @return A list containing the following components:
#' \item{matrix}{The adjacency matrix representation of the generated graph with edge capacities.}
#' \item{V}{The number of nodes in the graph (\code{num_nodes}).}
#' \item{E}{The number of edges in the graph (\code{num_edges}).}
#' \item{S}{The density of the graph, calculated as the ratio of the number of edges to the maximum possible number of edges.}
#' \item{graph}{The igraph object representing the generated graph.}
#' 
#' @details The function uses the Erdős-Rényi model to generate a random directed graph. 
#' The density of the graph is calculated as the ratio of the number of edges to the maximum possible 
#' number of edges in a directed graph with \code{num_nodes} nodes. 
#' The generated graph is represented as an adjacency matrix with edge capacities.
#' 
#' @seealso \code{\link{erdos.renyi.game}}, \code{\link{as_adjacency_matrix}}
#' 
#' @examples
#' # Generate a random directed graph with 10 nodes and 20 edges
#' graph_info <- gen_random_graph_adj_mat2(num_nodes = 10, num_edges = 20, capacity_range = c(1, 10))
#' print(graph_info)
gen_random_graph_adj_mat2 <- function(num_nodes, num_edges, capacity_range) {
  p <- num_edges / (num_nodes * (num_nodes - 1))
  graph <- erdos.renyi.game(num_nodes, p, directed = TRUE)
  E(graph)$capacity <- sample(capacity_range, ecount(graph), replace = TRUE)

  nombre_arcs <- ecount(graph)
  S <- nombre_arcs / (num_nodes * (num_nodes - 1))

  # cat("Densité du graphe :", S, "\n")

  adj_matrix <- as_adjacency_matrix(graph, attr = "capacity", sparse = FALSE, names = TRUE)

  return(list(matrix = adj_matrix, V = num_nodes, E = nombre_arcs, S = S,graph=graph))
}


#' Generate a random directed graph with adjacency matrix representation
#'
#' This function generates a random directed graph with adjacency matrix representation 
#' based on the Erdős-Rényi model. The graph is generated with a specified number of nodes 
#' and edge probability, and each edge is assigned a random capacity from a given range.
#'
#' @param num_nodes The number of nodes in the graph.
#' @param edge_prob The probability of an edge existing between any two nodes.
#' @param capacity_range A numeric vector specifying the range of capacities for the edges.
#' @return A list containing the following components:
#' \item{matrix}{The adjacency matrix representation of the generated graph with edge capacities.}
#' \item{V}{The number of nodes in the graph (\code{num_nodes}).}
#' \item{E}{The number of edges in the graph (\code{num_edges}).}
#' \item{S}{The density of the graph, calculated as the ratio of the number of edges to the maximum possible number of edges.}
#' \item{graph}{The igraph object representing the generated graph.}
#' 
#' @details The function uses the Erdős-Rényi model to generate a random directed graph. 
#' The density of the graph is calculated as the ratio of the number of edges to the maximum possible 
#' number of edges in a directed graph with \code{num_nodes} nodes. 
#' The generated graph is represented as an adjacency matrix with edge capacities.
#' 
gen_random_graph_adj_mat <- function(num_nodes, edge_prob, capacity_range) {
  graph <- erdos.renyi.game(num_nodes, edge_prob, directed = TRUE)
  E(graph)$capacity <- sample(capacity_range, ecount(graph), replace = TRUE)

  nombre_arcs <- ecount(graph)

  S=nombre_arcs/ (num_nodes*(num_nodes-1) )
  cat(S)

  adj_matrix <- as_adjacency_matrix(graph, attr = "capacity", sparse = FALSE, names = TRUE)
  arr=adj_matrix
  return(list(matrix=arr,V=num_nodes,E=nombre_arcs,S=S,graph=graph))
}





gen_random_graph_adj_Barabasi <- function(num_nodes, num_edges, capacity_range) {
  # Générer le graphe avec le modèle de préférence d'attachement (Barabási-Albert)
  graph <- igraph::sample_pa(n = num_nodes, m = num_edges, directed = TRUE)
  
  # Assigner des capacités aléatoires aux arêtes
  E(graph)$capacity <- sample(capacity_range, ecount(graph), replace = TRUE)
  
  # Calculer le nombre d'arêtes
  nombre_arcs <- ecount(graph)
  
  # Calculer la densité du graphe
  S <- nombre_arcs / (num_nodes * (num_nodes - 1))
  
  # Obtenir la matrice d'adjacence
  adj_matrix <- as_adjacency_matrix(graph, attr = "capacity", sparse = FALSE, names = TRUE)
  
  # Retourner une liste contenant les informations sur le graphe
  return(list(matrix = adj_matrix, V = num_nodes, E = nombre_arcs, S = S, graph = graph))
}




#' Generate a random directed small-world graph with adjacency matrix representation
#'
#' This function generates a random directed small-world graph with adjacency matrix representation 
#' based on the Watts-Strogatz model. The graph is generated with a specified number of nodes 
#' and number of edges, and each edge is assigned a random capacity from a given range.
#'
#' @param num_nodes Number of nodes
#' @param num_edges Number of edges
#' @param capacity_range Range of capacities for edges
#' @return A list with adjacency matrix, number of nodes, edges, density, and the graph object
gen_random_graph_adj_petitmonde <- function(num_nodes, num_edges, capacity_range) {
  # Calculer la probabilité de reconnexion basée sur le nombre d'arêtes
  prob_rewire <- num_edges / (num_nodes * (num_nodes - 1))
  
  # Générer le graphe de petit monde avec la probabilité de reconnexion calculée
  graph <- igraph::sample_smallworld(dim = 1, size = num_nodes, nei = 4, p = prob_rewire, loops = FALSE)
  
  # Convertir le graphe en un graphe dirigé
  graph <- igraph::as_directed(graph)
  
  # Assigner des capacités aléatoires aux arêtes
  E(graph)$capacity <- sample(capacity_range, ecount(graph), replace = TRUE)
  
  # Calculer le nombre d'arêtes
  nombre_arcs <- ecount(graph)
  
  # Calculer la densité du graphe
  S <- nombre_arcs / (num_nodes * (num_nodes - 1))
  
  # Obtenir la matrice d'adjacence
  adj_matrix <- as_adjacency_matrix(graph, attr = "capacity", sparse = FALSE, names = TRUE)
  
  # Retourner une liste contenant les informations sur le graphe
  return(list(matrix = adj_matrix, V = num_nodes, E = nombre_arcs, S = S, graph = graph))
}




