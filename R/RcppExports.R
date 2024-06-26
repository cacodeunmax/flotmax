# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#' Edmonds-Karp algorithm for maximum flow
#'
#' @description This function implements the Edmonds-Karp algorithm to find the maximum flow in a flow network.
#' @param capacity A NumericMatrix where the entry at (i, j) is the capacity of the edge from node i to node j.
#' @param source The index of the source node in the flow network (using 1-based indexing as in R).
#' @param sink The index of the sink node in the flow network (using 1-based indexing as in R).
#' @return A list containing two elements: the flow matrix and the value of the maximum flow.
#' @export
edmondsKarp <- function(capacity, source, sink) {
    .Call(`_flotmax_edmondsKarp`, capacity, source, sink)
}

#' Main function to implement Ford-Fulkerson algorithm
#'
#' @description Ford Fulkerson Algorithm to find maximum flow of a graph using a backtracking method to find the augmented path, The function to find augmented path is defined inside the main function and its callled find_augmenting_path. Note that Augmented path is recursively founded
#' @param graph A NumericMatrix is the adjacency matrix of a graph where the entry at (i, j) is the capacity of the edge from node i to node j.
#' @param source The index of the source node in the flow network (using 1-based indexing as in R).
#' @param sink The index of the sink node in the flow network (using 1-based indexing as in R).
#' @return It returns a list containing the flow matrix called residual matrix ( the way the flow should propagate) and the value of the maximum flow from the source.
#' @export
ford_fulkerson150 <- function(graph, source, sink) {
    .Call(`_flotmax_ford_fulkerson150`, graph, source, sink)
}

#' edmondsKarp_gif - Edmonds-Karp Algorithm for Maximum Flow
#'
#' This function implements the Edmonds-Karp algorithm to compute the maximum flow in a flow network.
#' It takes a graph represented by a capacity matrix, and computes the maximum flow from the source
#' to the sink. The function also captures the flow after each iteration, which can be used to create
#' a GIF visualization of the algorithm's progress.
#'
#' @param capacity A numeric matrix where `capacity[i, j]` represents the capacity of the edge from
#'                 node `i` to node `j`. If there is no edge between the nodes, the capacity should
#'                 be 0.
#' @param source An integer representing the source node in the flow network.
#' @param sink An integer representing the sink node in the flow network.
#'
#' @return A list containing two elements:
#'         - `matrices`: a vector of numeric matrices, each representing the state of the flow after
#'           each iteration of the algorithm.
#'         - `value`: the value of the maximum flow from the source to the sink.
NULL

edmondsKarp_gif <- function(capacity, source, sink) {
    .Call(`_flotmax_edmondsKarp_gif`, capacity, source, sink)
}

#' ford_fulkerson_gif - Ford-Fulkerson Algorithm for Maximum Flow
#'
#' This function implements the Ford-Fulkerson algorithm to find the maximum flow through a flow network.
#' It takes a graph represented by a capacity matrix and computes the max flow from the source to the sink node.
#' It records the residual graph after each iteration, which can be useful for visualizing the algorithm's progress.
#'
#' @param graph A numeric matrix representing the capacity graph where `graph[i, j]` is the capacity of the edge from
#'              node `i` to node `j`. If there is no edge, the capacity should be set to 0.
#' @param source An integer representing the index of the source node in the graph.
#' @param sink An integer representing the index of the sink node in the graph.
#'
#' @return A list with the following components:
#'         - `matrices`: A vector of numeric matrices representing the state of the residual graph after each
#'           iteration. This can be used to visualize the flow adjustments over time.
#'         - `value`: The numeric value of the maximum flow calculated by the algorithm.
#'
ford_fulkerson_gif <- function(graph, source, sink) {
    .Call(`_flotmax_ford_fulkerson_gif`, graph, source, sink)
}

