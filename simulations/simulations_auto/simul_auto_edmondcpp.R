
source("simulations/simulations_auto/gen_auto_network.R")
# library(RBGL)
# library(dlsem)

# Plages de valeurs pour n et max_cap
n_values_ <- c(1000) # plage de variation de n, le nb de noeuds
max_cap_values_ <- c(10) # plage de variation des capacités des arcs 
edge_proba_values_=c(0.1,0.15,0.2)
e_values=seq(from=3000,to=9000,by=300)


def_run_simu=function(n_iter=1,e_value=e_values_,n_values=n_values_,max_cap_values=max_cap_values_,edge_prob_values=edge_proba_values_,algo="fordR",controlE=F){
  folder_path <- "simulations/simulations_auto"
  file_path <- file.path(folder_path, "simulation_results_edmondcpp.csv")
  
  # décommanter pour ajouter les res à la fin
  # table.simu.fordR <- data.frame(E = integer(), f = numeric(), n = integer(), ExecutionTime = numeric())
  
  for (n in n_values) {
    
    for (max_cap in max_cap_values) {
      
      for (e in e_values){
        cat(e,'  ')
        for (i in 1:n_iter){
          
          capacity_range <- 1:max_cap
          # edge_prob=0.2
          
          if (controlE){
            res.gen= gen_random_graph_adj_mat2(n, e, capacity_range)
          }else{
            res.gen= gen_random_graph_adj_mat(n, edge_prob, capacity_range)
          }
          S=res.gen$S
          cat(S)
          adj_matrix=res.gen$matrix
          E=res.gen$E
          g=res.gen$graph
          
          # g_graphNEL <- as.graphNEL(g_igraph)
          
          # Exécuter ford_fulkerson et mesurer le temps
          start_time <- proc.time()
          res.ford <- edmondsKarp(adj_matrix, 1-1, n-1)
          execution_time <- proc.time() - start_time
          
          f <- res.ford$value
          
          results_line <- data.frame(E = E, f = f, n = n, ExecutionTime = execution_time["elapsed"])
          
          # Ajouter la ligne de résultats au fichier CSV, en ajoutant les noms de colonnes uniquement si le fichier n'existe pas
          write.table(results_line, file = file_path, append = TRUE, sep = ",", 
                      col.names = !file.exists(file_path) || -1 == file.info(file_path)$size, 
                      row.names = FALSE, quote = FALSE)
          
        }
      }
    }
  }
  
}
def_run_simu(n_iter = 10,controlE=T)
