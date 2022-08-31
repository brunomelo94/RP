exercice <- function(ex, method, eta, tol, mxepoc, trainPercentage) {
  
  if(ex==1) {
    n1 = 200;
    m11 = 2; 
    m12 = 2; 
    s1 = 0.4;
    
    n2 = 200; 
    m21 = 4; 
    m22 = 4; 
    s2 = 0.4
    
    c11 = rnorm(n1,m11,s1);
    c12 = rnorm(n1,m12,s1);
    c21 = rnorm(n2,m21,s2);
    c22 = rnorm(n2,m22,s2);
    
    Cp <- cbind(c11, c12) ##%classe positiva
    Cn <- cbind(c21, c22) ##%classe negativa
    
    y1 <- array(1,dim=c(n1,1));
    y2 <- array(-1,dim=c(n2,1));
    
    minYLabel = 0;
    
  } 
  else if(ex==2) {
    n1 = 200;
    m11 = -1; 
    m12 = -1; 
    s1 = 0.25;
    
    n2 = 200; 
    m21 = 1; 
    m22 = 1; 
    s2 = 0.25
    
    n3 = 200; 
    m31 = -1; 
    m32 = 1; 
    s3 = 0.25
    
    n4 = 200; 
    m41 = 1; 
    m42 = -1; 
    s4 = 0.25
      
    c11 = rnorm(n1,m11,s1);
    c12 = rnorm(n1,m12,s1);
    c21 = rnorm(n2,m21,s2);
    c22 = rnorm(n2,m22,s2);
    c31 = rnorm(n3,m31,s3);
    c32 = rnorm(n3,m32,s3);
    c41 = rnorm(n4,m41,s4);
    c42 = rnorm(n4,m42,s4);
    
    Cp <- rbind(cbind(c11, c12), cbind(c21, c22)) ##%classe positiva
    Cn <- rbind(cbind(c31, c32), cbind(c41, c42)) ##%classe 
    
    y1 <- array(1,dim=c(n1+n2,1));
    y2 <- array(-1,dim=c(n3+n4,1));
    
    minYLabel = -2.5;
  }
  
  Yp = y1;
  Yn = y2
  
  X = rbind(Cp,Cn)
  Y = rbind(Yp,Yn)
  
  plot(Cp[,1],Cp[,2],col='green',pch='o', 
       xlim = c(minYLabel,max(Cp[,1],Cn[,1])),ylim = c(minYLabel,max(Cp[,2],Cn[,2])),xlab = 'x1' ,ylab= 'x2',)
  
  par(new=T)
  
  plot(Cn[,1],Cn[,2],col='blue',pch='+', 
       xlim = c(minYLabel,max(Cp[,1],Cn[,1])),ylim = c(minYLabel,max(Cp[,2],Cn[,2])),xlab = 'x1' ,ylab= 'x2',)
  
  if(method == 'adaline') {
    retlist<-trainadaline(X,Y,eta,tol,mxepoc,1)
    
  } else if (method == 'perceptron') {
    retlist<-trainadalinePerceptron(X,Y,eta,tol,mxepoc,1)
  }
  
  w<-retlist[[1]]
  erro<-retlist[[2]]
  plot(erro,type='l', xlab="Época")
  
  seqi<-seq(minYLabel,6,0.05)
  seqj<-seq(minYLabel,6,0.05)
  
  M <- matrix(0,nrow=length(seqi),ncol=length(seqj)) 
  ci<-0
  for (i in seqi){
    ci<-ci+1
    cj<-0
    for(j in seqj)
    {
      cj<-cj+1
      
      if(method == 'adaline') {
        M[ci,cj]<- yadaline(cbind(i,j),w,1)
      } else if (method == 'perceptron') {
        M[ci,cj]<- yPerceptron(cbind(i,j),w,1)
      }
      
    }
  }
  
  plot(Cp[,1],Cp[,2],col='green',pch='o', xlim = c(minYLabel,max(Cp[,1],Cn[,1])),ylim = c(minYLabel,max(Cp[,2],Cn[,2])),xlab = 'x1' ,ylab= 'x2',)
  
  par(new=T)
  
  plot(Cn[,1],Cn[,2],col='blue',pch='+', xlim = c(minYLabel,max(Cp[,1],Cn[,1])),ylim = c(minYLabel,max(Cp[,2],Cn[,2])),xlab = 'x1' ,ylab= 'x2',)
  
  par(new=T)
  
  contour2D(M,seqi,seqj,xlim = c(minYLabel,max(Cp[,1],Cn[,1])),ylim = c(minYLabel,max(Cp[,2],Cn[,2])),xlab = 'x1' ,ylab= 'x2',levels=0 )
  
  persp3D(seqi,seqj,M,counter=T,theta = 55, phi = 30, r = 40, d = 0.1, expand = 0.5,
          ltheta = 90, lphi = 180, shade = 0.4, ticktype = "detailed", nticks=5)
  
  if(ex == 1) {
    idx = sample(n1+n2)
    tam = round(trainPercentage * (n1 + n2))
    
  } else if (ex == 2) {
    idx = sample(n1+n2+n3+n4)
    tam = round(trainPercentage * (n1 + n2 + n3 + n4))
  }
  
  Xtrain = X[idx[1:tam],]   
  Ytrain = Y[idx[1:tam],]
  
  if(method == 'adaline') {
    retlist<-trainadaline(Xtrain,Ytrain,eta,tol,mxepoc,1)
    
  } else if (method == 'perceptron') {
    retlist<-trainadalinePerceptron(Xtrain,Ytrain,eta,tol,mxepoc,1)
  }
  
  w<-retlist[[1]]
  erro<-retlist[[2]]
  
  #Yhat = yadaline(X,w,1)
  plot(erro,type='l', xlab="Época")
  
  #separação
  seqi<-seq(minYLabel,6,0.06)
  seqj<-seq(minYLabel,6,0.06)
  
  M <- matrix(0,nrow=length(seqi),ncol=length(seqj)) 
  ci<-0
  
  for (i in seqi){
    ci<-ci+1
    cj<-0
    for(j in seqj)
    {
      cj<-cj+1
      
      if(method == 'adaline') {
        M[ci,cj]<- yadaline(cbind(i,j),w,1)
      } else if (method == 'perceptron') {
        M[ci,cj]<- yPerceptron(cbind(i,j),w,1)
      }
      
    }
  }
  
  plot(Cp[,1],Cp[,2],col='green',pch='o', xlim = c(minYLabel,max(Cp[,1],Cn[,1])),ylim = c(minYLabel,max(Cp[,2],Cn[,2])),xlab = 'x1' ,ylab= 'x2',)
  
  par(new=T)
  
  plot(Cn[,1],Cn[,2],col='blue',pch='+', xlim = c(minYLabel,max(Cp[,1],Cn[,1])),ylim = c(minYLabel,max(Cp[,2],Cn[,2])),xlab = 'x1' ,ylab= 'x2',)
  
  par(new=T)
  
  plot(Xtrain[,1],Xtrain[,2],col='green',pch='0', xlim = c(minYLabel,max(Cp[,1],Cn[,1])),ylim = c(minYLabel,max(Cp[,2],Cn[,2])),xlab = 'x1' ,ylab= 'x2',)
  
  par(new=T)
  
  contour2D(M,seqi,seqj,xlim = c(minYLabel,max(Cp[,1],Cn[,1])),ylim = c(minYLabel,max(Cp[,2],Cn[,2])),xlab = 'x1' ,ylab= 'x2',levels=0 )
  
  #return (list(w,erro))
}