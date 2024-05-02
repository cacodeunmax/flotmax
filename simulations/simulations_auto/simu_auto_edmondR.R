
source("simulations/simulations_auto/gen_auto_network.R")


# Plages de valeurs pour n et max_cap
n_values_ <- c(20,50,100,200) # plage de variation de n, le nb de noeuds
max_cap_values_ <- c(2,3,4,5,10) # plage de variation des capacités des arcs 
edge_proba_values_=c(0.1,0.2)


def_run_simu=function(n_iter=1,n_values=n_values_,max_cap_values=max_cap_values_,edge_prob_values=edge_proba_values_,algo="fordR"){
  folder_path <- "simulations/simulations_auto"
  file_path <- file.path(folder_path, "simulation_results_edmondR.csv")
  
  # décommanter pour ajouter les res à la fin
  # table.simu.fordR <- data.frame(E = integer(), f = numeric(), n = integer(), ExecutionTime = numeric())
  
  for (n in n_values) {
    
    for (max_cap in max_cap_values) {
      
      for (edge_prob in edge_prob_values){
        
        for (i in 1:n_iter){
          
          capacity_range <- 1:max_cap
          # edge_prob=0.2
          
          res.gen= gen_random_graph_adj_mat(n, edge_prob, capacity_range)
          adj_matrix=res.gen$matrix
          E=res.gen$E
          
          # Exécuter ford_fulkerson et mesurer le temps
          start_time <- proc.time()
          res.ford <- edmonds_karp(adj_matrix, 1, n)
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
def_run_simu(n_iter = 10)
