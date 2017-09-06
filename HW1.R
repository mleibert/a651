rm(list=ls())
options(stringsAsFactors = FALSE) 
options(scipen=999)

 
setwd("G:/math/651")
gpa<-read.table("gradepointaverage.txt")
head(gpa)

colnames(gpa)<-c("Y","X")

b1 = sum( (gpa$X-mean(gpa$X))*(gpa$Y-mean(gpa$Y)))/ sum( (gpa$X-mean(gpa$X))^2 )
b1
b0 = mean(gpa$Y)-b1*mean(gpa$X)
b0 
lm( gpa$Y ~ gpa$X)

plot(gpa$X,gpa$Y)
abline(lm( gpa$Y ~ gpa$X), col="red")

b0+b1*30

