log.reg2<-glm(Survived~.,data=train2.df,family = "binomial")
options(scipen=999)
#summary+odds
summary(log.reg2)
odds=exp(coef(log.reg2))


#predictions for the first model getting rid of the entries in the validation dataset that have unknwn elements


log.reg.pred2<-predict(log.reg2,valid2.df,type="response")

#evaluation of the results
comparison2<-data.frame(actual=results2.df,predicted=log.reg.pred2)
head(comparison1)
#lift chart
library(gains)
gain<-gains(results2.df,log.reg.pred1)
plot(c(0,gain$cume.pct.of.total*sum(results1.df))~
       c(0,gain$cume.obs),
     xlab="# cases", ylab="Survivors", main="", type="l")
lines(c(0,sum(results2.df))~c(0, length(results2.df)), lty=2)
#confusion matrix
confusionMatrix(as.factor(ifelse(log.reg.pred2>0.5,1,0)),as.factor(results2.df))
#decline-wise lift chart
heights<-gain$mean.resp/mean(results2.df)
midpoints<-barplot(heights,names.arg = gain$depth, ylim=c(0,3), xlab="Percentile",ylab="Prediction effectiveness",main="Decline-wise Lift Chart")
text(midpoints,heights+0.1, labels=round(heights,1),cex=0.8)

