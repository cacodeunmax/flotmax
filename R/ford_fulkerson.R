#' ford_fulkerson: Ford fulkerson main function
#'
#' Main function to implement Ford-Fulkerson algorithm
#'
#' @description Ford Fulkerson Algorithm to find maximum flow of a graph using a backtracking method to find the augmented path, The function to find augmented path is defined inside the main function and its callled find_augmenting_path. Note that Augmented path is recursively founded.
#'
#' @param graph is the adjacency matrix of a graph where the entry at (i, j) is the capacity of the edge from node i to node j.
#' @param source The index of the source node in the flow network (using 1-based indexing as in R).
#' @param sink The index of the sink node in the flow network (using 1-based indexing as in R).
#' @return It returns a list containing the flow matrix called residual matrix ( the way the flow should propagate) and the value of the maximum flow from the source.




ford_fulkerson <- function(graph, source, sink) {
  # Initialize residual graph with capacities same as the original graph
  residual <- graph
  max_flow <- 0
  
  # Function to find augmenting path using backtracking
  find_augmenting_path <- function(residual, current, sink, path = c()) {
    path <- c(path, current)
    if (current == sink) {
      return(path)
    }
    
    for (neighbor in 1:nrow(residual)) {
      if (residual[current, neighbor] > 0 && !(neighbor %in% path)) {
        augmenting_path <- find_augmenting_path(residual, neighbor, sink, path)
        if (!is.null(augmenting_path)) {
          return(augmenting_path)
        }
      }
    }
    return(NULL)
  }
  
  # Main loop of Ford-Fulkerson algorithm
  while (TRUE) {
    # Find augmenting path using backtracking
    augmenting_path <- find_augmenting_path(residual, source, sink)
    
    # If no augmenting path is found, terminate
    if (is.null(augmenting_path)) {
      break
    }
    #cat("Augmenting path found:", augmenting_path, "\n")
    
    # Find the minimum residual capacity along the augmenting path
    path_flow <- Inf
    for (i in 1:(length(augmenting_path)-1)) {
      u <- augmenting_path[i]
      v <- augmenting_path[i+1]
      path_flow <- min(path_flow,residual[u,v])
      
    }  
    # Update residual capacities along the augmenting path
    for (i in 1:(length(augmenting_path) - 1)) {
      u <- augmenting_path[i]
      v <- augmenting_path[i + 1]
      residual[u, v] <- residual[u, v] - path_flow
      residual[v, u] <- residual[v, u] + path_flow
    }
    
    # Add path flow to overall max flow
    max_flow <- max_flow + path_flow
    #print(max_flow)
  }
  matrix=graph-residual
  # Return the maximum flow and the residual graph
  return(list(matrix = matrix,value = max_flow))
}
