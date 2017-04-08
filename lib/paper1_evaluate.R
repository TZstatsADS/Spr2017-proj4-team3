evaluate1=function(true.authorID,cluster.authorID)
{ 
  m=length(true.authorID)
for(i in 1:m)
{ for (j in 1:m)
{ if(j>i)
{ if(i==1 & j==2)
{x=ifelse(true.authorID[[i]]==true.authorID[[j]],"match","mismatch")
y=ifelse(cluster.authorID[[i]]==cluster.authorID[[j]],"match","mismatch")}
}
  else
  {x=c(x,ifelse(true.authorID[[i]]==true.authorID[[j]],"match","mismatch"))
  y=c(y,ifelse(cluster.authorID[[i]]==cluster.authorID[[j]],"match","mismatch"))}
}
}
  return(table(x,y))
}