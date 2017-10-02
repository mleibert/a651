rm(list=ls())
options(stringsAsFactors = FALSE) 
options(scipen=999)
setwd("G:/math/651")

#5.2


#5.5
x<-c(4,1,2,3,3,4)
y<-c(16,5,10,15,13,22)

X<-data.matrix(data.frame(rep(1,length(x)), x));colnames(X) <- NULL
Y<-matrix(y,6,1)
X
Y


sum(Y^2)
t(Y) %*% Y
t(X) %*% X
t(X) %*% Y

paste("(", X[,2] ,"-",round(mean(X[,2]),8),")^2+",sep="")

sum((X[,2]-mean(X[,2]))^2)
6*sum((X[,2]-mean(X[,2]))^2)

solve(t(X) %*% X)

ab<-read.table("AirfreightBreakage.txt")
colnames(ab)<-c("Y","X")
head(ab)
ab$one<-1

X<-data.matrix(ab[,3:2]);colnames(X) <- NULL
Y<-data.matrix(ab[,1]);colnames(Y) <- NULL


### Y'Y

t(Y) %*% Y

### X'X

t(X) %*% X

### X'Y

t(X) %*% Y


##

B<-matrix(c(1,1,1,5,0,0,0,5,5),3,3,byrow=F)

det(B)


### 5.15

A=matrix(c(5,2,23,7),2,2,byrow=T)
Y=matrix(c(8,28),2,1 )
solve(A,Y)

###### 5.24
x<-c(4,1,2,3,3,4)
y<-c(16,5,10,15,13,22)

X<-data.matrix(data.frame(rep(1,length(x)), x));colnames(X) <- NULL
Y<-matrix(y,6,1)
X
Y
H<-X%*%solve(t(X)%*%X)%*%t(X)
YH<-H%*%Y
n=6
J<-matrix(rep(1,36),6,6)

#est reg coeff
b<-solve(t(X)%*%X)%*%t(X)%*%Y;b

#vect of resid
e<-Y-YH;e

#SSR
t(b)%*%t(X)%*%Y-(1/n)*t(Y)%*%J%*%Y
anova(lm(y~x))

#SSE
t(e)%*%e

var( solve(t(X)%*%X) )

var(e)
