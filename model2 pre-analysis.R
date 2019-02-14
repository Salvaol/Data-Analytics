#logisitc mdodel 2

#if we explore the correlations table there seems to be a noticeable correlation between the fare and the embarking port
mean(train1.df[train1.df$Embarked=="C",]$Fare)
mean(train1.df[train1.df$Embarked=="S",]$Fare)
mean(train1.df[train1.df$Embarked=="Q",]$Fare)
#as well as with the fare abd the ticket class
mean(train1.df[train1.df$Pclass==1,]$Fare)
mean(train1.df[train1.df$Pclass==2,]$Fare)
mean(train1.df[train1.df$Pclass==3,]$Fare)
#age seems to be more important to survival than the correlations table would suggest
#lets consider a new ranged partition for a more efficient model
#the youngest individual seems to have a higher survival rate
#let's study what should be considered young
compare.young<-data.frame(age=integer(),survived=integer(),total=integer(),per100=numeric())
for (i in 1:30) {
  young1<-train1.df[train1.df$Age<i,]
  total1<-dim(young1)[1]
  survived1<-sum(young1$Survived)
  per100.1<-survived1/total1*100
  compare.young[i,]$age<-i
  compare.young[i,]$survived<-survived1
  compare.young[i,]$total<-total1
  compare.young[i,]$per100<-per100.1
}
print(compare.young)
#there's a major drop starting in % from 8 to 9 years, let's set the category kid until 8
#another pattern that seems to exist is that elderly don't seem to survive
compare.elderly<-data.frame(age=integer(),survived=integer(),total=integer(),per100=numeric())
for (i in 50:79) {
  elderly1<-train1.df[train1.df$Age>i,]
  total1<-dim(elderly1)[1]
  survived1<-sum(elderly1$Survived)
  per100.1<-survived1/total1*100
  compare.elderly[i,]$age<-i
  compare.elderly[i,]$survived<-survived1
  compare.elderly[i,]$total<-total1
  compare.elderly[i,]$per100<-per100.1
}
print(compare.elderly)
#3 conclusions can be extracted from this analysis
#1st: People above 62 years could be considered elderly as survival gets considerably reduced after that
#2nd: The entry of 80 years is an outlier, thus removing it may improve performance
#3rd: Age as a number seems to hold a relation with the survival, but adding categories kid and elderly may improve performance
#
#
#
#PREPROCESSING
#getting rid of eldest entry
rownames(train.df[train.df$Age==80,])
#getting rid of the people that go in first class not paying fare (crew)
crew<-as.integer(rownames(train.df[((train.df$Fare==0) & (train.df$Pclass==1)),]))
train2.df<-train.df[c(-unk.age,-62.-830,-631,-crew),]

max(train2.df$Age)
#create elderly and kid variables
#train2.df$kid<-ifelse(train2.df$Age<9,1,0)
#train2.df$elderly<-ifelse(train2.df$Age>62,1,0)
valid2.df<-valid.df[-1*unk.age.valid,]
#valid2.df$kid<-ifelse(valid2.df$Age<9,1,0)
#valid2.df$elderly<-ifelse(valid2.df$Age>62,1,0)
valid2.df$Pclass<-as.factor(valid2.df$Pclass)
results2.df<-results.df[-unk.age.valid,]
results2.df<-results2.df[,-1]
train2.df$Pclass<-as.factor(train2.df$Pclass)
valid2.df<-valid2.df[,c(-2,-7,-9)]
train2.df<-train2.df[,c(-3,-8,-10)]
train2.df$Survived<-as.factor(train2.df$Survived)
