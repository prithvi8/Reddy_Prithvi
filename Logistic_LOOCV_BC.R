df<-read.csv("BreastCancer.csv")
df1<-df[,-1]
mod<-glm(Class~.,family=binomial,data=df1)
summary(mod)

prob<- predict(mod, df, type="response")
logit.pred<-factor(prob>0.5, levels=c(FALSE, TRUE), labels=c("benign","malignant"))

logit.perf <- table(df1$Class, logit.pred, dnn = c("Actual", "Predicted") )
logit.perf


isCorrect <- NULL
#LOOCV
for( i in 1:nrow(df1))
{
  x.train<-df1[-i,]
  x.test <- df1[i,]
  
  mod<-glm(Class ~ ., x.train,family="binomial")
  pred<- predict(mod, newdata=x.test, type="response")
  y.pred<- ifelse(pred > 0.5,1,0)
  y.true<- x.test$Class
  isCorrect[i]<-ifelse(y.pred == y.true,1,0)
  
}

#CV misclassification error
isCorrect <- na.omit(isCorrect)
sum(isCorrect)/length(isCorrect)
  
  
  
  
  
  
  
  
  


  