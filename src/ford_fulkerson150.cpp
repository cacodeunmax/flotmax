#include <Rcpp.h>
using namespace Rcpp;

//' Main function to implement Ford-Fulkerson algorithm
//'
//' @description Ford Fulkerson Algorithm to find maximum flow of a graph using a backtracking method to find the augmented path, The function to find augmented path is defined inside the main function and its callled find_augmenting_path. Note that Augmented path is recursively founded
//' @param graph A NumericMatrix is the adjacency matrix of a graph where the entry at (i, j) is the capacity of the edge from node i to node j.
//' @param source The index of the source node in the flow network (using 1-based indexing as in R).
//' @param sink The index of the sink node in the flow network (using 1-based indexing as in R).
//' @return It returns a list containing the flow matrix called residual matrix ( the way the flow should propagate) and the value of the maximum flow from the source.
//' @export
// [[Rcpp::export]]
List ford_fulkerson150(NumericMatrix graph, int source, int sink) {
  int n = graph.nrow();
  NumericMatrix residual = clone(graph);
  double max_flow = 0;
  
  // Function to find augmenting path using backtracking
  std::function<std::vector<int>(NumericMatrix, int, int, std::vector<bool>, std::vector<int>)> find_augmenting_path = [&](NumericMatrix residual, int current, int sink, std::vector<bool> visited, std::vector<int> path) {
    path.push_back(current);
    if (current == sink) {
      return path;
    }
    
    for (int neighbor = 0; neighbor < residual.nrow(); ++neighbor) {
      if (residual(current, neighbor) > 0 && !visited[neighbor]) {
        visited[neighbor] = true;
        std::vector<int> augmenting_path = find_augmenting_path(residual, neighbor, sink, visited, path);
        if (!augmenting_path.empty()) {
          return augmenting_path;
        }
        visited[neighbor] = false;
      }
    }
    return std::vector<int>();
  };
  
  // Main loop of Ford-Fulkerson algorithm
  while (true) {
    // Initialize visited vector
    std::vector<bool> visited(n, false);
    visited[source - 1] = true; // Mark source node as visited
    
    // Find augmenting path using backtracking
    std::vector<int> augmenting_path = find_augmenting_path(residual, source - 1, sink - 1, visited, std::vector<int>());
    
    // If no augmenting path is found, terminate
    if (augmenting_path.empty()) {
      break;
    }
    
    // Find the minimum residual capacity along the augmenting path
    double path_flow = R_PosInf;
    for (int i = 0; i < static_cast<int>(augmenting_path.size()) - 1; ++i) {
      int u = augmenting_path[i];
      int v = augmenting_path[i + 1];
      path_flow = std::min(path_flow, residual(u, v));
    }
    
    // Update residual capacities along the augmenting path
    for (int i = 0; i < static_cast<int>(augmenting_path.size()) - 1; ++i) {
      int u = augmenting_path[i];
      int v = augmenting_path[i + 1];
      residual(u, v) -= path_flow;
      residual(v, u) += path_flow;
    }
    
    // Add path flow to overall max flow
    max_flow += path_flow;
  }
  
  List result = List::create(Named("matrix") = (graph-residual), Named("value") = max_flow);
  return result;
}
