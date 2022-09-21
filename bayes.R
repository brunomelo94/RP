<<>>=
  
rm(list = ls())

# Amostragem dos dados
xc1 <- matrix(rnorm(100), ncol = 2) + 2

xc2 <- matrix(rnorm(100), ncol = 2) + 6

plot(xc1[,1], xc1[,2], col = 'blue' , xlim = c(0, 10), ylim = c(0, 10))

par(new = T)

plot(xc2[,1], xc2[,2], col = 'red' , xlim = c(0, 10), ylim = c(0, 10))

xall <- rbind(xc1, xc2)

yall <- rbind( matrix(1, nrow = 50), matrix(2, nrow = 50) )

###########################################################################

seqxc1 <- which(yall == 1)
seqxc2 <- which(yall == 2)

N1 <- length(seqxc1)
N2 <- length(seqxc2)

pc1 <- N1/(N1 + N2)
pc2 <- N2/(N1 + N2)

X1 <- xall[seqxc1, ]
X2 <- xall[seqxc2, ]

### Classe 1
m11 <- colMeans(X1)[1]
m12 <- colMeans(X1)[2]

s11 <- sd(X1[,1])
s12 <- sd(X1[,2])

### Classe 2
m21 <- colMeans(X2)[1]
m22 <- colMeans(X2)[2]

s21 <- sd(X2[,1])
s22 <- sd(X2[,2])

rbfGivar <- function(x,m,r) ( (1/sqrt(2*pi*r*r)) * exp(-0.5 * (x-m)^2 / (2*r^2)) )

###########################################################

xrange <- seq(0, 10 , 0.1)

fx11 <- rbfGivar(xrange, m11, s11)
fx12 <- rbfGivar(xrange, m12, s12)

par(new = T)

plot(xrange, 5*fx11, type = 'l', col = 'blue', xlim = c(0, 10), ylim = c(0, 10))

par(new = T)

plot(5*fx12, xrange, type = 'l', col = 'blue', xlim = c(0, 10), ylim = c(0, 10))

fx21 <- rbfGivar(xrange, m21, s21)
fx22 <- rbfGivar(xrange, m22, s22)

par(new = T)

plot(xrange, 5*fx21, type = 'l', col = 'red', xlim = c(0, 10), ylim = c(0, 10))

par(new = T)

plot(5*fx22, xrange, type = 'l', col = 'red', xlim = c(0, 10), ylim = c(0, 10))


