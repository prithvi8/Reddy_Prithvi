ad<-read.csv("Advertising.csv")[,-1]

df<-ad%>%select(TV,Sales)


ssquares<- function(x)
{ n <- nrow(df)
  sum((df[,2] - cbind(1,df)))
  
}


sum(df)


mod<-lm(Sales~TV,df)