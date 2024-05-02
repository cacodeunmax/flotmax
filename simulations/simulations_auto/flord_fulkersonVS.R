
source("simulations/simulations_auto/gen_auto_network.R")


# Plages de valeurs pour n et max_cap
n_values_ <- seq(from=30,to=30,by=1) # plage de variation de n, le nb de noeuds
max_cap_values_ <- c(5) # plage de variation des capacités des arcs 
edge_proba_values_=c(0.1,0.2)
e_values_=seq(from=80,to=80,by=1)


def_run_simu=function(n_iter=1,e_values=e_values_,n_values=n_values_,max_cap_values=max_cap_values_,edge_prob_values=edge_proba_values_,algo="fordR",controlE=F){
  folder_path <- "simulations/simulations_auto"
  file_path <- file.path(folder_path, "simulation_results_fordVS_Efixe.csv")

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
          
          
          # Exécuter ford_fulkerson R
          start_time <- proc.time()
          res.ford <- ford_fulkerson(adj_matrix, 1, n)
          execution_time <- proc.time() - start_time
          
          f <- res.ford$value
          
          results_line_R <- data.frame(E = E, f = f, n = n, ExecutionTime = execution_time["elapsed"],cpp=0,max_cap=max_cap)
          
          
          # Exécuter ford_fulkerson cpp
          start_time <- proc.time()
          res.ford <- ford_fulkerson150(adj_matrix, 1, n)
          execution_time <- proc.time() - start_time
          
          f <- res.ford$value
          results_line_cpp <- data.frame(E = E, f = f, n = n, ExecutionTime = execution_time["elapsed"],cpp=1,max_cap=max_cap)
          results_line=rbind(results_line_R,results_line_cpp)
          
          # Ajouter la ligne de résultats au fichier CSV, en ajoutant les noms de colonnes uniquement si le fichier n'existe pas
          write.table(results_line, file = file_path, append = TRUE, sep = ",", 
                      col.names = !file.exists(file_path) || -1 == file.info(file_path)$size, 
                      row.names = FALSE, quote = FALSE)
          
        }
      }
    }
  }
  
}
def_run_simu(n_iter = 50,e_values_,n_values_,max_cap_values_,edge_prob_values_,controlE=T)
