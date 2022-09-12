
rm(list=ls())

# Duas classes gaussianas.

x1<-replicate(2,rnorm(20,mean=5,sd=0.8))
x2<-replicate(2,rnorm(20,mean=8,sd=0.4))

y1 = array(1,c(20,1))
y2 = y1*(-1)



plot(x1[,1],x1[,2],col="red",xlim=c(0,10),ylim=c(0,10))
points(x2[,1],x2[,2],col="blue")


X = rbind(x1,x2)
Y = rbind(y1,y2)


xt = c(7.5,7)

points(xt[1],xt[2],col="green")


k=5

source("myknnR.R")
result = myknnR(X,Y,xt,k)


###############################################
# exercício


s1<-0.3
s2<-0.3
s3<-0.3
s4<-0.3
nc<-100
xc1<-matrix(rnorm(nc*2),ncol=2)*s1 +
  t(matrix(c(2,2),nrow=2,ncol=nc))
xc2<-matrix(rnorm(nc*2),ncol=2)*s2 +
  t(matrix(c(4,4),nrow=2,ncol=nc))
xc3<-matrix(rnorm(nc*2),ncol=2)*s3 +
  t(matrix(c(2,4),nrow=2,ncol=nc))
xc4<-matrix(rnorm(nc*2),ncol=2)*s4 +
  t(matrix(c(4,2),nrow=2,ncol=nc))

y1 = array(1,c(nc,1))
y2 = array(2,c(nc,1))
y3 = array(3,c(nc,1))
y4 = array(4,c(nc,1))



plot(xc1[,1],xc1[,2],col="red",xlim=c(0,8),ylim=c(0,8))
points(xc2[,1],xc2[,2],col="blue")
points(xc3[,1],xc3[,2],col="green")
points(xc4[,1],xc4[,2],col="brown")


nc=20
s1 = 2

xt<-matrix(runif(nc*2)-0.5,ncol=2)*s1 +
  t(matrix(c(3,3),nrow=2,ncol=nc))

points(xt[,1],xt[,2],col="black")


X = rbind(xc1,xc2,xc3,xc4)
Y = rbind(y1,y2,y3,y4)



plot(xc1[,1],xc1[,2],col="red",xlim=c(0,8),ylim=c(0,8))
points(xc2[,1],xc2[,2],col="blue")
points(xc3[,1],xc3[,2],col="green")
points(xc4[,1],xc4[,2],col="brown")

