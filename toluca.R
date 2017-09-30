toluca<-read.table("Toluca.txt")
colnames(toluca)<-c("X","Y")
toluca$Yhat<-as.numeric(lm(toluca$Y~toluca$X)[[1]][1]) +
	as.numeric(lm(toluca$Y~toluca$X)[[1]][2]) * toluca$X
toluca$ei<-toluca$Y-toluca$Yhat
toluca$eiRank<-rank(toluca$ei)
anova(lm(toluca$X~toluca$Y))
toluca.n<-nrow(toluca)

toluca.MSE<-sum(toluca$ei^2)/(nrow(toluca)-2)
toluca$Eoei<-sqrt(toluca.MSE)*(
	qnorm((  (toluca$eiRank-.375) / (nrow(toluca)+.25) )	))
cor(toluca$ei,toluca$Eoei)
qnorm(toluca$ei)

dotchart(toluca$X)

ggplot(gpa, aes(x = X)) + geom_dotplot( ) + theme_bw() 


paste("e_",1:10,"=(" ,
	ab$Y,"-",round(ablm(ab$X),4), ")", " = " ,round(ab$ei,4),sep="")

plot(ab$X,ab$ei)

summary(	lm(toluca$Y~toluca$X)	)

anova(	lm(toluca$Y~toluca$X)	)


2384*((1/nrow(toluca)) + (100-mean(toluca$X))^2/(sum(
	 (toluca$X-mean(toluca$X))^2 )) )

sqrt(203)

62.37 +    3.57  * 100

toluca.lm<-function(X){lm(toluca$Y~toluca$X)[[1]][1] +
	 lm(toluca$Y~toluca$X)[[1]][2] *X }

lm(toluca$Y~toluca$X)[[1]][1]
lm(toluca$Y~toluca$X)[[1]][2]

toluca.ssb1<-toluca.MSE*(1/sum((toluca$X-mean(toluca$X))^2))
sqrt(toluca.ssb1)

toluca.ssb0<-toluca.MSE* ((1/toluca.n) + ((mean(toluca$X)^2)/  
	( sum((toluca$X-mean(toluca$X))^2)	 ) ) )
sqrt(toluca.ssb0)

#confidence level?
CL<-.9
alpha<-(1-CL);alpha
cl<-qt(1-alpha/4,toluca.n-2);cl

