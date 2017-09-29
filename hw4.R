A=matrix(0,4,2)
A[,1]<-c(2,3,5,4)
A[,2]<-c(1,5,7,8)
A

B=matrix(0,4,1)
B[,1]<-c(6,3,9,1)

C=matrix(0,4,2)
C[,1]<-c(3,8,5,2)
C[,2]<-c(8,6,1,4)
C


A-C

A %*% t(B)


#####
x<-c(4, 1, 2, 3, 3, 4)
Y<-c(16, 5, 10, 15, 13, 22)
X<-matrix(0,length(x),2);X[,1]<-1;X[,2]<-x;X
Y<-as.matrix(Y)

as.matrix(X)%*%t(as.matrix(X))
pmatrix(t(X),digits=0)

pmatrix(t(Y),digits=0)
pmatrix(Y,digits=0)
pmatrix(Y%*%t(Y),digits=0)

pmatrix(t(X),digits=0)
pmatrix(Y,digits=0)
pmatrix(t(X)%*%Y,digits=0)

