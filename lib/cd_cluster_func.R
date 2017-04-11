combinecluster<-function(cluster){
  M = list()
  for(i in 1:length(cluster)){
    for(j in 1:length(cluster[[i]])){
      M[cluster[[i]][j]] = i
    }
  }
  for(i in 1:length(M)){
    if(M[i] == "NULL"){
      M[i] = 0  
    }
  }
  
  G = list()
  for(i in 1:length(data_list[[k]])){
    G[i]=data_list[[k]][[i]][1]

  }
  if(length(M) < length(G)){
    for(i in (length(M)+1:length(G)))
      M[i] = 0
  }
  
  M <- as.data.frame(M)
  G <- as.data.frame(G)
  
  
  result_matrix <- matching_matrix(G, M)
  print(result_matrix)
  performace <-performance_statistics(result_matrix)
  return(list(result_matrix, performace))
}

deletecluster<-function(cluster){
  G = list()
  for(i in 1:length(data_list[[k]])){
    G[i]=data_list[[k]][[i]][1]
  }
  M = list()
  for(i in 1:length(cluster)){
    for(j in 1:length(cluster[[i]])){
      M[cluster[[i]][j]] = i
    }
  }
  if(length(M) < length(G)){
    for(i in (length(M)+1:length(G)))
      M[i] = 0
  }
  
  for(i in 1:length(M)){
    if(M[i] == "NULL"){
      M[i] = 0  
    }
  }
  x = 0
  M = t(as.matrix(M))
  deletel <- which(!is.na(match(M, x)))
  M = M[,-c(deletel)]

  G <- t(as.matrix(G))
  G = G[,-c(deletel)]
  
  M <- as.data.frame(M)
  G <- as.data.frame(G)
  
  result_matrix <- matching_matrix(G, M)
  print(result_matrix)
  performance <- performance_statistics(result_matrix) 
  return(list(result_matrix, performance))
}
