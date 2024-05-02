

#include <Rcpp.h>
#include <queue>
using namespace Rcpp;




//' Edmonds-Karp algorithm for maximum flow
//'
//' @description This function implements the Edmonds-Karp algorithm to find the maximum flow in a flow network.
//' @param capacity A NumericMatrix where the entry at (i, j) is the capacity of the edge from node i to node j.
//' @param source The index of the source node in the flow network (using 1-based indexing as in R).
//' @param sink The index of the sink node in the flow network (using 1-based indexing as in R).
//' @return A list containing two elements: the flow matrix and the value of the maximum flow.
//' @export
// [[Rcpp::export]]
List edmondsKarp(NumericMatrix capacity, int source, int sink) {
  int n = capacity.nrow();
  NumericMatrix flow(n, n);
  IntegerVector parent(n);
  double max_flow = 0;
  
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
  }
  
  // Create a list to return both the flow matrix and the max flow value
  List result = List::create(Named("matrix") = flow, Named("value") = max_flow);
  
  return result;
}
