dim(train.df)
unk.age<-c(unk.age,62,830)
train3.df<-train.df[-c(unk.age),]
dim(train3.df)

train3.df<-train3.df[,c(-3,-8,-10)]
rownames(train3.df[train3.df$Embarked=="",])

as.integer(rownames(train3.df[is.na(train1.df$Age),]))

train3.df$Pclass<-as.factor(train3.df$Pclass)###
train3.df$Age<-NULL
train3.df$SibSp<-NULL
train3.df$Parch<-NULL
train3.df$Embarked<-NULL
train3.df$Fare<-NULL
log.reg3<-glm(Survived~.,data=train3.df,family = "binomial")
options(scipen=999)
#summary+odds
summary(log.reg3)
sum<-summary(log.reg3)
variables.df<-as.array(sum$coefficients)
write.csv(variables.df, file = "coefficientEs.csv")

#predictions for the first model getting rid of the entries in the validation dataset that have unknwn elements
unk.age.valid<-as.integer(rownames(valid.df[is.na(valid.df$Age),]))
rownames(valid.df[valid.df$Embarked=="",])
unk.age.valid<-unk.age.valid-891
valid3.df<-valid.df[-1*unk.age.valid,]
valid3.df$Pclass<-as.factor(valid1.df$Pclass)########


results3.df<-results.df[-unk.age.valid,]
results3.df<-results3.df[,-1]
log.reg.pred3<-predict(log.reg3,valid3.df[,c(-2,-4,-5,-6,-7,-8,-9,-10)],type="response")

#evaluation of the results
comparison3<-data.frame(actual=results3.df,predicted=log.reg.pred3)
head(comparison1)

#confusion matrix
library(caret)
confusionMatrix(as.factor(ifelse(log.reg.pred3>0.5,1,0)),as.factor(results3.df))
#lift chart
library(gains)
gain<-gains(results3.df,log.reg.pred3)
plot(c(0,gain$cume.pct.of.total*sum(results3.df))~
       c(0,gain$cume.obs),
     xlab="# cases", ylab="Survivors", main="", type="l")
lines(c(0,sum(results3.df))~c(0, length(results3.df)), lty=2)
#
#decline-wise lift chart
heights<-gain$mean.resp/mean(results3.df)
midpoints<-barplot(heights,names.arg = gain$depth, ylim=c(0,3), xlab="Percentile",ylab="Prediction effectiveness",main="Decline-wise Lift Chart")
text(midpoints,heights+0.1, labels=round(heights,1),cex=0.8)
