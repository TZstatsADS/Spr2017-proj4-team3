cluster1=function(data,theta=1)
{

  #initialize cluster
cluster=list()
for( i in 1:n)
{cluster[[i]]=i}

 #initialize intersect matrix
n=length(coauthor)
inter=matrix(0,nrow=n,ncol=n)
for (i in 1:n)
  for (j in 1:n)
  { if(i!=j)
    {inter[i,j]=length(intersect(coauthor[[i]],coauthor[[j]]))}
  }

#iterate
while(max(inter)>=theta)
{
  u=which(inter==max(inter),arr.ind = T)[1,1]
  v=which(inter==max(inter),arr.ind = T)[1,2]
  cluster[[v]]=union(cluster[[v]],cluster[[u]])
  cluster[[u]]=NULL
  inter[v,]=pmax(inter[u,],inter[v,])
  inter[,v]=inter[v,]
  inter=inter[-u,]
  inter=inter[,-u]
  diag(inter)=0
}
return(cluster)
}