#logistic regression model 1
#get rid of vbles with unknowns and use all predictor vbles
#logistic model without the entries where the age and the port is unknown
#including the fare and the class

dim(train.df)
unk.age<-c(unk.age,62,830)
train1.df<-train.df[-c(unk.age),]
dim(train1.df)

train1.df<-train1.df[,c(-3,-8,-10)]
rownames(train1.df[train1.df$Embarked=="",])

as.integer(rownames(train1.df[is.na(train1.df$Age),]))

train1.df$Pclass<-as.factor(train1.df$Pclass)###
log.reg1<-glm(Survived~.,data=train1.df,family = "binomial")
options(scipen=999)
#summary+odds
sum<-summary(log.reg1)
variables.df<-as.array(sum$coefficients)
write.csv(variables.df, file = "coefficients1.csv")
odds=exp(coef(log.reg1))
odds
write.csv(variables.df, file = "odds.csv")

#predictions for the first model getting rid of the entries in the validation dataset that have unknwn elements
unk.age.valid<-as.integer(rownames(valid.df[is.na(valid.df$Age),]))
rownames(valid.df[valid.df$Embarked=="",])
unk.age.valid<-unk.age.valid-891
valid1.df<-valid.df[-1*unk.age.valid,]
valid1.df$Pclass<-as.factor(valid1.df$Pclass)########
results1.df<-results.df[-unk.age.valid,]
results1.df<-results1.df[,-1]
log.reg.pred1<-predict(log.reg1,valid1.df[,c(-2,-7,-9)],type="response")

#evaluation of the results
comparison1<-data.frame(actual=results1.df,predicted=log.reg.pred1)
head(comparison1)

#confusion matrix
library(caret)
confusionMatrix(as.factor(ifelse(log.reg.pred1>0.5,1,0)),as.factor(results1.df))

#lift chart
library(gains)
gain<-gains(results1.df,log.reg.pred1)
plot(c(0,gain$cume.pct.of.total*sum(results1.df))~
       c(0,gain$cume.obs),
     xlab="# cases", ylab="Survivors", main="", type="l")
lines(c(0,sum(results1.df))~c(0, length(results1.df)), lty=2)
#
#decline-wise lift chart
heights<-gain$mean.resp/mean(results1.df)
midpoints<-barplot(heights,names.arg = gain$depth, ylim=c(0,3), xlab="Percentile",ylab="Prediction effectiveness",main="Decline-wise Lift Chart")
text(midpoints,heights+0.1, labels=round(heights,1),cex=0.8)

