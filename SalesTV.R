ad<-read.csv("Advertising.csv")[,-1]
mod<-lm(Sales~TV,ad)
res<-residuals(mod)
plot(res~predict(mod),main="Residual Plot",xlab="observation",ylab="Residuals")

X<-ad[,1]
Y<-ad[,4]

X<-cbind(1,X)

a<-t(X)%*%X
b<-(t(X)%*%Y)

beta<-solve(a,b)

lm(Sales~TV,ad)
beta