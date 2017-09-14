rm(list=ls())
options(stringsAsFactors = FALSE) 
options(scipen=999)

 
setwd("G:/math/651")

#1.19
gpa<-read.table("gradepointaverage.txt")
head(gpa)
colnames(gpa)<-c("Y","X")

b0<-as.numeric(lm(gpa$Y~gpa$X)[[1]][1]) ;b0
b1<-as.numeric(lm(gpa$Y~gpa$X)[[1]][2]) ;b1
gpalm<-function(X){X*b1+b0 }


n=nrow(gpa)
gpa$ei<-(gpa$Y-gpalm(gpa$X))
sum(gpa$ei)
gpa$eiei<-gpa$ei^2
MSE<-sum(gpa$eiei)/(n-2);MSE
gpa$SR<-(gpalm(gpa$X)-mean(gpa$Y))^2
SSR<-sum(gpa$SR);SSR

#a
anova(lm(gpa$Y~gpa$X))

#b