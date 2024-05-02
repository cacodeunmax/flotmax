#include <Rcpp.h>
#include <queue>
using namespace Rcpp;

//' edmondsKarp_gif - Edmonds-Karp Algorithm for Maximum Flow
//'
//' This function implements the Edmonds-Karp algorithm to compute the maximum flow in a flow network.
//' It takes a graph represented by a capacity matrix, and computes the maximum flow from the source
//' to the sink. The function also captures the flow after each iteration, which can be used to create
//' a GIF visualization of the algorithm's progress.
//'
//' @param capacity A numeric matrix where `capacity[i, j]` represents the capacity of the edge from
//'                 node `i` to node `j`. If there is no edge between the nodes, the capacity should
//'                 be 0.
//' @param source An integer representing the source node in the flow network.
//' @param sink An integer representing the sink node in the flow network.
//'
//' @return A list containing two elements:
//'         - `matrices`: a vector of numeric matrices, each representing the state of the flow after
//'           each iteration of the algorithm.
//'         - `value`: the value of the maximum flow from the source to the sink.

// [[Rcpp::export]]
List edmondsKarp_gif(NumericMatrix capacity, int source, int sink) {
  int n = capacity.nrow();
  NumericMatrix flow(n, n);
  IntegerVector parent(n);
  double max_flow = 0;
  std::vector<NumericMatrix> flows; // To store flow after each iteration
  
  auto bfs = [&]() -> bool {
    std::fill(parent.begin(), parent.end(), -1);
    parent[source] = -2;
    std::queue<int> q;
    q.push(source);
    
    while (!q.empty()) {
      int u = q.front(); q.pop();
      
      for (int v = 0; v < n; ++v) {
        if (parent[v] == -1 && capacity(u, v) - flow(u, v) > 0) {
          parent[v] = u;
          q.push(v);
          if (v == sink) return true;
        }
      }
    }
    return false;
  };
  
  while (bfs()) {
    double path_flow = R_PosInf;
    for (int v = sink; v != source; v = parent[v]) {
      int u = parent[v];
      path_flow = std::min(path_flow, capacity(u, v) - flow(u, v));
    }
    for (int v = sink; v != source; v = parent[v]) {
      int u = parent[v];
      flow(u, v) += path_flow;
      flow(v, u) -= path_flow;
    }
    max_flow += path_flow;
    flows.push_back(clone(flow)); // Clone and store the current state of the flow matrix
  }
  
  // Create a list to return all the flow matrices and the max flow value
  List result = List::create(Named("matrices") = wrap(flows), Named("value") = max_flow);
  
  return result;
}
