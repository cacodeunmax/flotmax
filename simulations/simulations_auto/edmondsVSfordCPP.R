library(igraph)
source("simulations/simulations_auto/gen_auto_network.R")

# Plages de valeurs pour n et max_cap
n_values_ <- seq(20) # plage de variation de n, le nb de noeuds
max_cap_values_ <- c(3) # plage de variation des capacités des arcs 
edge_proba_values_ <- c(0.5)
e_values_ <- seq(from = 50, to = 50, by = 5)

def_run_simu <- function(n_iter = 1, e_values = e_values_, n_values = n_values_, max_cap_values = max_cap_values_, edge_prob_values = edge_proba_values_, algo = "fordR", controlE = FALSE) {
  folder_path <- "simulations/simulations_auto"
  file_path <- file.path(folder_path, "simulation_results_fordVSedmondsCPP.csv")
  
  for (n in n_values) {
    for (max_cap in max_cap_values) {
      for (e in e_values) {
        for (edge_prob in edge_prob_values){
        cat(e, '  ')
        for (i in 1:n_iter) {
          
          capacity_range <- 1:max_cap
          
          if (controlE) {
            res.gen <- gen_random_graph_adj_mat2(n, e, capacity_range)
          } else {
            res.gen <- gen_random_graph_adj_mat(n, edge_prob, capacity_range)
          }
          
          S <- res.gen$S
          adj_matrix <- res.gen$matrix
          E <- res.gen$E
          
          # Exécuter edmonds cpp
          start_time <- proc.time()
          res.ford <- edmondsKarp(adj_matrix, 1 - 1, n - 1)
          execution_time_edmonds <- proc.time() - start_time
          f <- res.ford$value
          results_line_ed <- data.frame(E = E, f = f, n = n, ExecutionTime = execution_time_edmonds["elapsed"], edmonds = 1, max_cap = max_cap)
          
          # Exécuter ford_fulkerson cpp avec limite de temps
          # setTimeLimit(elapsed = 180, transient = TRUE)
          start_time <- proc.time()
          # tryCatch({
          res.ford <- ford_fulkerson150(adj_matrix, 1, n)
          # }, error = function(e) {
            # execution_time_ford <- c(elapsed = -1, user = -1, system = -1)
          # })
          execution_time_ford <- proc.time() - start_time
          results_line_ford <- data.frame(E = E, f = f, n = n, ExecutionTime = execution_time_ford["elapsed"], edmonds = 0, max_cap = max_cap)
          
          # Ajouter la ligne de résultats au fichier CSV
          write.table(results_line_ed, file = file_path, append = TRUE, sep = ",", col.names = !file.exists(file_path) || -1 == file.info(file_path)$size, row.names = FALSE, quote = FALSE)
          write.table(results_line_ford, file = file_path, append = TRUE, sep = ",", col.names = FALSE, row.names = FALSE, quote = FALSE)
        }
        }
      }
    }
  }
}

def_run_simu(n_iter = 3, e_values = e_values_, n_values = n_values_, max_cap_values = max_cap_values_, edge_prob_values = edge_proba_values_, controlE = F)
