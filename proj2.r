library(MASS)
x<-cars
x<-scale(x)
output <-cov.mcd(x,quantile.used = nrow(x)*.75)
mf <-mahalanobis(x,output$center,output$cov)
alpha <-.01
cutoff <-(qchisq(p = 1-alpha, df = 2))
excluded<-which(mf > cutoff)
x2 <-x[-excluded,]
plot(-3:3,-3:3,type="n")

for(i in 1:nrow(x))
{
  if(i %in% c(excluded))
    points(x[i,1],x[i,2],col="red")
  
  else
    points(x[i,1],x[i,2],col="black")
  
}
plot(x)
X<-x[-excluded,1]
Y<-x[-excluded,2]
Y1<-x[,2]
X1<-x[,1]
abline(lm(Y~X))
m1=as.data.frame.array(x)
m2=m1
fit1=lm(Y~X,data=m1[-excluded,])
fit2=lm(Y1~X1,data=m2)
summary(fit1)
summary(fit2)
