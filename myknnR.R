myknnR<-function(X,Y,xt,k)
# X: dataset (Nxn)
# Y: labels (Nx1)
# xt: test vector (nx1)
  #para duas classes
{
  N<-dim(X)[1]
  n<-dim(X)[2]
  
  seqi<-seq(1,N,1)
  mdist<-matrix(nrow=N,ncol=1)
  for(i in seqi)
  {
    xc<-X[i,]
    mdist[i]<-sqrt(sum((xc-t(xt))^2))
  }
  
  ordmdist<-order(mdist)
  ordY<-Y[ordmdist]
   
  yxt<-sign(sum(ordY[1:k]))
  
  retlist<-list(yxt,mdist,ordY)
  
  return(retlist)  
    
}