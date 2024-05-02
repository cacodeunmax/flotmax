#include <Rcpp.h>
using namespace Rcpp;

//' ford_fulkerson_gif - Ford-Fulkerson Algorithm for Maximum Flow
//'
//' This function implements the Ford-Fulkerson algorithm to find the maximum flow through a flow network.
//' It takes a graph represented by a capacity matrix and computes the max flow from the source to the sink node.
//' It records the residual graph after each iteration, which can be useful for visualizing the algorithm's progress.
//'
//' @param graph A numeric matrix representing the capacity graph where `graph[i, j]` is the capacity of the edge from
//'              node `i` to node `j`. If there is no edge, the capacity should be set to 0.
//' @param source An integer representing the index of the source node in the graph.
//' @param sink An integer representing the index of the sink node in the graph.
//'
//' @return A list with the following components:
//'         - `matrices`: A vector of numeric matrices representing the state of the residual graph after each
//'           iteration. This can be used to visualize the flow adjustments over time.
//'         - `value`: The numeric value of the maximum flow calculated by the algorithm.
//'
// [[Rcpp::export]]
List ford_fulkerson_gif(NumericMatrix graph, int source, int sink) {
  int n = graph.nrow();
  NumericMatrix residual = clone(graph);
  double max_flow = 0;
  std::vector<NumericMatrix> flows; // To store flow after each iteration
  std::vector<std::vector<int>> augmenting_paths; // To store augmenting paths
  
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
    
    // Store augmenting path
    augmenting_paths.push_back(augmenting_path);
    
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
    flows.push_back(clone(residual)); // Clone and store the current state of the flow matrix
  }
  
  List result = List::create(Named("matrices") = wrap(flows), Named("value") = max_flow, Named("augmenting_paths") = wrap(augmenting_paths));
  
  return result; 
}

