source("../lib/paper1_cluster.R")
source("../lib/evaluation_measures.R")
paper1_indiv<-function(k){
  table<-list()
  eval<-list()
  n=length(data_list[[k]])  
  coauthor <<-list()
  authorID <<- list()
  for(i in 1:n)
  {
    coauthor[[i]] <<- data_list[[k]][[i]][[3]]  #coauthor list for cluster
    authorID[[i]] <<- data_list[[k]][[i]][[1]]  #authorID list for evaluate
  }
  cluster=cluster1(coauthor)
  
  #generate clusterID
  len=length(cluster)
  clusterID=rep(0,n)
  for(i in 1:n)
  { for (j in 1:len)
  {
    if( !is.na(match(i,cluster[[j]])) ) 
    {clusterID[i]=j;break}
  }
  }
  table<-matching_matrix(unlist(authorID),clusterID)
  eval<-performance_statistics(table)
  return(list(table, eval))
}