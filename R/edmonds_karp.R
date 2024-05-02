#' bfs_edmonds: Breadth-First Search
#'
#' This function performs a Breadth-First Search (BFS) on a graph represented by an adjacency matrix.
#'
#' @param arr The adjacency matrix representing the graph.
#' @param dp The residual matrix.
#' @param source The index of the source node.
#' @param sink The index of the sink node.
#' @return A vector representing the first augmenting path found by the BFS.
bfs_edmonds <- function(arr, dp, source, sink) {
  q <- c(source)
  paths <- list()
  paths <- purrr::assign_in(paths, as.character(source), NULL)
  if (source == sink) {
    return(paths[[as.character(source)]])
  }
  
  while (length(q) > 0) {
    u <- q[1]
    if (length(q) != 1) {
      q <- q[-1]
    } else {
      q <- c()
    }
    
    for (neigh in 1:ncol(arr)) {
      if (arr[u, neigh] - dp[u, neigh] > 0 & is.null(paths[[as.character(neigh)]])) {
        if (is.null(purrr::pluck(paths, as.character(u)))) {
          val <- c(u, neigh)
        } else {
          prev <- purrr::pluck(paths, as.character(u))
          val <- c(u, neigh)
          val <- c(prev, val)
        }
        paths <- purrr::assign_in(paths, as.character(neigh), val)
        
        if (neigh == sink) {
          return(purrr::pluck(paths, as.character(neigh)))
        }
        
        q <- c(q, neigh)
      }
    }
  }
  return(NULL)
}

#' get_flow_min: Get the minimum residual capacity of a certain path
#'
#' This function calculates the value of the augmentation of the augmenting path
#'
#' @param path A vector representing an augmenting path.
#' @param arr The adjacency matrix representing the network and current flow passing through it.
#' @param dp The residual matrix representing the remaining capacities in the network.
#' @return The value by which the path can be augmented.
get_flow_min <- function(path, arr, dp) {
  vmin <- +Inf
  for (i in seq(1, length(path), by = 2)) {
    x1 <- path[i]
    x2 <- path[i + 1]
    v <- arr[x1, x2] - dp[x1, x2]
    vmin <- min(v, vmin)
  }
  return(vmin)
}

#' edmonds_karp: Edmonds-Karp Algorithm for Maximum Flow
#'
#' This function implements the Edmonds-Karp algorithm to find the maximum flow in a network represented by an adjacency matrix.
#' It utilizes the Breadth-First Search (BFS) algorithm and the get_flow_min function to iteratively find augmenting paths and update flow values.
#'
#' @param arr The adjacency matrix representing the network.
#' @param source The index of the source node.
#' @param sink The index of the sink node.
#' @return A list containing the final flow matrix and the value of the maximum flow.
edmonds_karp <- function(arr, source, sink) {
  n <- ncol(arr)
  dp <- matrix(0, n, n)
  path <- bfs_edmonds(arr, dp, source, sink)
  
  while (!is.null(path)) {
    flow <- get_flow_min(path, arr, dp)
    for (i in seq(1, length(path), by = 2)) {
      u <- path[i]
      v <- path[i + 1]
      dp[u, v] <- dp[u, v] + flow
      dp[v, u] <- dp[v, u] - flow
    }
    path <- bfs_edmonds(arr, dp, source, sink)
  }
  
  return(list(matrix = dp, value = sum(dp[source, ])))
}
