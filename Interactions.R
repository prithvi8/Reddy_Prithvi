ad<-read.csv("Advertising.csv")[,-1]
lm(Sales~TV+Radio,ad)
int1<-(ad$Sales*ad$TV)
int2<-ad$Sales*ad$Radio
int3<-ad$TV*ad$Radio
lm(Sales~TV+Radio+int1+int2+int3,ad)

"The Interaction terms are significant in changing the effect of Radio on Sales.Coefficients on the
interaction terms are not significant. Coefficient on TV is not affected"