#data_list from clean_data.rmd
cd_cluster <- function(txtNumber){
#initial co-author 
n=length(data_list[[txtNumber]])  #2nd name


#initial intersect matrix
newlist=list()
for(i in 1:n)
{
        newlist[[i]]=data_list[[txtNumber]][[i]][[3]]  #2nd name, change with n
}

inter=matrix(0,nrow=n,ncol=n+1)
for (i in 1:n)
        for (j in 1:n)
        { inter[i,j]=length(intersect(newlist[[i]],newlist[[j]]))
        }
diag(inter)=0

inter[,n+1] <- seq(1:n)

index <- which(rowSums(inter[,-(n+1)]) == 0)
inter <- inter[-c(index),-c(index)]


#initial cluster
cluster=list()
for( i in 1:nrow(inter))
{cluster[[i]]=inter[i,nrow(inter)+1]}
        
#interate
inter <- inter[,-(nrow(inter)+1)]
while(max(inter)>=1)
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

combinecluster(c2_cluster(2))
deletecluster(c2_cluster(2))
