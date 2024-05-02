# Implémenter Dinitz en R












# dinitz_max_flow <- function(capacity_matrix, source, sink) {
#   n <- nrow(capacity_matrix)  # Nombre de sommets
#   max_flow <- 0  # Initialiser le flot maximal à zéro
#   augmenting_paths <- list()  # Initialiser la liste des chemins augmentants
#   
#   # Fonction pour trouver un chemin augmentant
#   find_augmenting_path <- function(residual_graph) {
#     visited <- rep(FALSE, n)  # Marquer tous les sommets comme non visités
#     parent <- rep(-1, n)      # Tableau pour stocker les parents des sommets dans le chemin
#     
#     queue <- list(source)
#     visited[source] <- TRUE
#     
#     # Parcours BFS
#     while (length(queue) > 0) {
#       u <- queue[[1]]
#       queue <- queue[-1]
#       for (v in 1:n) {
#         if (!visited[v] && residual_graph[u, v] > 0) {
#           parent[v] <- u
#           visited[v] <- TRUE
#           queue <- c(queue, v)
#         }
#       }
#     }
#     
#     # Retourner le chemin trouvé
#     if (visited[sink]) {
#       path <- c()
#       current <- sink
#       while (current != -1) {
#         path <- c(current, path)
#         current <- parent[current]
#       }
#       return(path)
#     } else {
#       return(NULL)  # Aucun chemin trouvé
#     }
#   }
#   
#   # Fonction pour augmenter le flot le long du chemin trouvé
#   augment_flow <- function(residual_graph, path, delta) {
#     for (i in 1:(length(path) - 1)) {
#       residual_graph[path[i], path[i + 1]] <- residual_graph[path[i], path[i + 1]] - delta
#       residual_graph[path[i + 1], path[i]] <- residual_graph[path[i + 1], path[i]] + delta
#     }
#     return(residual_graph)
#   }
#   
#   # Boucle principale pour trouver les chemins augmentants
#   while (TRUE) {
#     # Trouver un chemin augmentant
#     augmenting_path <- find_augmenting_path(capacity_matrix)
#     if (is.null(augmenting_path)) {
#       break  # Si aucun chemin n'est trouvé, arrêter
#     }
#     
#     # Ajouter le chemin trouvé à la liste des chemins augmentants
#     augmenting_paths <- c(augmenting_paths, list(augmenting_path))
#     
#     # Calculer la capacité résiduelle minimale le long du chemin
#     min_capacity <- Inf
#     for (i in 1:(length(augmenting_path) - 1)) {
#       u <- augmenting_path[i]
#       v <- augmenting_path[i + 1]
#       min_capacity <- min(min_capacity, capacity_matrix[u, v])
#     }
#     
#     # Augmenter le flot le long du chemin trouvé
#     capacity_matrix <- augment_flow(capacity_matrix, augmenting_path, min_capacity)
#     
#     # Mettre à jour le flot maximal
#     max_flow <- max_flow + min_capacity
#   }
#   
#   return(list(max_flow, augmenting_paths))
# }
