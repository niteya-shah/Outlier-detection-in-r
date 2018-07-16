library('philentropy')
x<-cars
plot(x)
x<-scale(x)
plot(x);
N=3
nearest<-list();
densiti<-vector();
meanm<-vector();
r3<-vector();
devm<-vector();
dop<-vector();

y=2;
w<-distance(x, method = "euclidean")

minN <- function(x, N=1){
  len <- length(x)
  if(N>len){
    warning('N greater than length(x).  Setting N=length(x)')
    N <- length(x)
  }
  sort(x)[N]
}

for(i in 1:nrow(w))
{ nearest[[i]]=c();
#nearest[[i]][1]<-minN(w[i,],2);

  for(j in 2:(N+1))
{r3[(j-1)]<-minN(w[i,],j);}
nearest[[i]]<-r3

    # nearest[[i]]<-c(minN(w[i,],2),minN(w[i,],3),minN(w[i,],4));
densiti[i]<-c(1/(sum(nearest[[i]])));
meanm[i]<-c((1/N)*sum(nearest[[i]]));
devm[i]<-c(sqrt(((1/(nrow(w)-1))*sum((nearest[[i]]-meanm[i])^2))));  
}

avgden<-sum(densiti)
for(i in 1:nrow(w))
{
  dop[i]<-(0.811/avgden)*(((meanm[i]+devm[i])^y)/(densiti[i]*(((meanm[i]+devm[i])^y)-(meanm[i]-devm[i])^y)))
}
excluded<-vector();
plot(-3:3,-3:3,type="n")
xb<-vector();
for(i in 1:nrow(w))
{
  if(dop[i]>0.3){
    points(x[i,1],x[i,2],col="red");
    excluded<-append(excluded,i)
    }
    else{
    points(x[i,1],x[i,2],col="black")
  
      }
}
plot(x[-excluded,])
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
