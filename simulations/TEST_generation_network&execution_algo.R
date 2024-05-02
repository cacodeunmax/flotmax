library(igraph)
# Charger la bibliothèque igraph

# Fonction pour générer un graphe aléatoire avec des capacités aléatoires
generate_random_graph <- function(num_nodes, edge_prob, capacity_range) {
  graph <- erdos.renyi.game(num_nodes, edge_prob, directed = TRUE)
  E(graph)$capacity <- sample(capacity_range, ecount(graph), replace = TRUE)
  return(graph)
}



# Définir les paramètres
num_nodes <- 10

edge_prob <- 0.2
capacity_range <- 1:8

cat ( num_nodes*(num_nodes-1) * edge_prob)

# Générer un graphe aléatoire avec des capacités aléatoires
graph <- generate_random_graph(num_nodes, edge_prob, capacity_range)

nombre_arcs <- ecount(graph)
cat(nombre_arcs)

S=nombre_arcs/ (num_nodes*(num_nodes-1) )
cat(S)
# Afficher le graphe




plot(graph, edge.label = E(graph)$capacity, edge.arrow.size = 0.5, layout = layout.auto(graph))
adj_matrix <- as_adjacency_matrix(graph, attr = "capacity", sparse = FALSE, names = TRUE)

arr=adj_matrix

# Définir une fonction pour formater le texte des étiquettes des arêtes
format_edge_labels <- function(graph) {
  capacities <- E(graph)$capacity
  formatted_labels <- paste0("0 / ", capacities)
  return(formatted_labels)
}

# Afficher le graphe avec les étiquettes des arêtes formatées
plot(graph, edge.label = format_edge_labels(graph), edge.arrow.size = 0.5, layout = layout.auto(graph))






gen_random_graph_adj_mat <- function(num_nodes, num_edges, capacity_range) {
  p <- num_edges / (num_nodes * (num_nodes - 1))
  graph <- erdos.renyi.game(num_nodes, p, directed = TRUE)
  E(graph)$capacity <- sample(capacity_range, ecount(graph), replace = TRUE)
  
  nombre_arcs <- ecount(graph)
  S <- nombre_arcs / (num_nodes * (num_nodes - 1))
  
  cat("Densité du graphe :", S, "\n")
  
  adj_matrix <- as_adjacency_matrix(graph, attr = "capacity", sparse = FALSE, names = TRUE)
  
  return(list(matrix = adj_matrix, V = num_nodes, E = nombre_arcs, S = S,graph=graph))
}

# Exemple d'utilisation
random_graph <- gen_random_graph_adj_mat(num_nodes = 10, num_edges = 30, capacity_range = 1:10)
adj_matrix=random_graph$matrix






n <- ncol(arr)
dp <- matrix(0, n, n)




a=flotmax::edmonds_karp(adj_matrix, 1, 10)



V=num_nodes
E=nombre_arcs





tps1 <- system.time(edmonds_karp(adj_matrix, 1, sink = num_nodes))
print(tps1)

flowmax=edmondsKarp(adj_matrix, 1 - 1, num_nodes - 1)$value
cat(E*flowmax)

tps2 <- system.time(edmondsKarp(adj_matrix, 1 - 1, num_nodes - 1))
print(tps2)



tps3 <- system.time(ford_fulkerson(adj_matrix, 1, num_nodes))
print(tps3)


tps4 <- system.time(ford_fulkerson150(adj_matrix, 1, num_nodes))
print(tps4)
