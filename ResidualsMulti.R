ad<-read.csv("Advertising.csv")[,-1]
mod<-lm(Sales~TV,ad)
res<-residuals(mod)
plot(res~predict(mod),main="Residual Plot",xlab="observation",ylab="Residuals")

X<-ad[,1:3]
Y<-ad[,4]

X<-cbind(1,X)

X= as.matrix(as.data.frame(lapply(X, as.numeric)))

a<-t(X)%*%X
b<-(t(X)%*%Y)

beta<-solve(a,b)



H<-X%*%solve(t(X)%*%X)%*%t(X)
I<-diag(200)
Res<-(I-H)%*%Y
plot(Res~TV,main="Residual Plot",xlab="TV_Observation",ylab="Residuals",ad)
plot(Res~Radio,main="Residual Plot",xlab="Radio_Observation",ylab="Residuals",ad)
plot(Res~Newspaper,main="Residual Plot",xlab="Newspaper_Observation",ylab="Residuals",ad)