rm(list=ls())
options(stringsAsFactors = FALSE) 
options(scipen=999)
setwd("G:/math/651")

pt<-read.table("productiontime.txt")
colnames(pt)<-c("Y","X")
head(pt)

#a
plot(pt$X,pt$Y)
#the data appear somewhat curvlinear. A transformation on X, such as 
#$sqrt{X}$ might be appropriate. $sqrt{Y}$ may materially change the shape 
#of the distribution of the error terms from normal and could lead to 
#differing error term variances.

#b
pt$Xp<-sqrt(pt$X)
lm(pt$Y~pt$X)
pt.b0p<-as.numeric(lm(pt$Y~pt$Xp)[[1]][1])
pt.b1p<-as.numeric(lm(pt$Y~pt$Xp)[[1]][2])

# $1.254697+3.62352X$

#c
plot(pt$Xp,pt$Y)
abline(lm(pt$Y~pt$Xp),col="red")

#e
pt.lm<-function(X){pt.b0p + pt.b1p*X}
pt$eieip<-pt$Y - pt.lm(pt$Xp)
plot(pt$eieip,pt.lm(pt$Xp))
qqnorm(pt$eieip)

#f
#$(pt.b0p+pt.b1p*X)^2$


bp<-read.table("BloodPressure.txt")
colnames(bp)<-c("Y","X")
head(bp)
lm(bp$Y~bp$X)
n=nrow(bp)

bp.b0<-as.numeric(lm(bp$Y~bp$X)[[1]][1])
bp.b1<-as.numeric(lm(bp$Y~bp$X)[[1]][2])
bp.lm<-function(X){bp.b0 + bp.b1*X}

bp$ei<-bp$Y - bp.lm(bp$X)
plot(bp$X,bp$ei)
#not enough points to make a judgement. One notable outlier.

#b
lm(bp$Y~bp$X)
bp<-bp[-7,]
n=nrow(bp)
bp.b0<-as.numeric(lm(bp$Y~bp$X)[[1]][1])
bp.b1<-as.numeric(lm(bp$Y~bp$X)[[1]][2])
bp.lm<-function(X){bp.b0 + bp.b1*X}
lm(bp$Y~bp$X)
bp.lm<-function(X){bp.b0 + bp.b1*X}
bp$ei<-bp$Y - bp.lm(bp$X)

bp.lm(12)
bp.mse<-sum(bp$ei^2)/(n-2)
anova(lm(bp$Y~bp$X))

Xh<-12
bp.sspred<-bp.mse*(1+(1/n)+ ( Xh-mean(bp$X))^2 / 
	( sum( (bp$X-mean(bp$X))^2 ) ) )
bp.spred<-sqrt(bp.sspred)

#confidence level?
CL<-.99
alpha<-(1-CL)/2;alpha
cl<-qt(1-alpha,n-2);cl


bp.lm(12)-cl*bp.spred;bp.lm(12)+cl*bp.spred
#Y_7 does fall otuside this predicition interval...




##############	4.3

#a b0 and b1

cm<-read.table("CopierMaintenance.txt")
colnames(cm)<-c("Y","X")
tail(cm)
lm(cm$Y~cm$X)

cm.n<-nrow(cm)
cm.lm<-function(X){lm(cm$Y~cm$X)[[1]][1] + lm(cm$Y~cm$X)[[1]][2] *X }

cm.b0<-lm(cm$Y~cm$X)[[1]][1]
cm.b1<-lm(cm$Y~cm$X)[[1]][2]

cm$ei<-cm$Y-cm.lm(cm$X)
cm.mse<-sum(cm$ei^2) / (cm.n-2);cm.mse

cm.ssb1<- cm.mse*(1/sum((cm$X-mean(cm$X))^2))
cm.ssb1

#The covariance between b0 and b1 is negative and that implies that the 
#esimators b0 and b1 tend to err in opposite directions

cm.ssb0<-cm.mse * ((1/cm.n) + ((mean(cm$X)^2)/  ( sum((cm$X-mean(cm$X))^2)
	 ) ) )
cm.ssb0

#confidence level?
CL<-.95
alpha<-(1-CL);alpha
cl<-qt(1-alpha/4,cm.n-2);cl

cm.b1-sqrt(cm.ssb1)*cl;cm.b1+sqrt(cm.ssb1)*cl
cm.b0-sqrt(cm.ssb0)*cl;cm.b0+sqrt(cm.ssb0)*cl

##############	4.5

#a b0 and b1

ph<-read.table("PlasticHardness.txt")
colnames(ph)<-c("Y","X")
tail(ph)
lm(ph$Y~ph$X)

ph.n<-nrow(ph)
ph.lm<-function(X){lm(ph$Y~ph$X)[[1]][1] + lm(ph$Y~ph$X)[[1]][2] *X }

ph.b0<-as.numeric(lm(ph$Y~ph$X)[[1]][1])
ph.b1<-as.numeric(lm(ph$Y~ph$X)[[1]][2])

ph$ei<-ph$Y-ph.lm(ph$X)
ph.mse<-sum(ph$ei^2) / (ph.n-2);ph.mse

ph.ssb1<- ph.mse*(1/sum((ph$X-mean(ph$X))^2))
ph.ssb1

ph.ssb0<-ph.mse * ((1/ph.n) + ((mean(ph$X)^2)/  ( sum((ph$X-mean(ph$X))^2)
	 ) ) )
ph.ssb0

#confidence level?
CL<-.9
alpha<-(1-CL);alpha
cl<-qt(1-alpha/4,ph.n-2);cl

ph.b1-sqrt(ph.ssb1)*cl;ph.b1+sqrt(ph.ssb1)*cl
ph.b0-sqrt(ph.ssb0)*cl;ph.b0+sqrt(ph.ssb0)*cl



