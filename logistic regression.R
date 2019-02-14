#data has already been provided in training and validation datasets
train.df<-read.csv("train.csv")
valid.df<-read.csv("test.csv")
results.df<-read.csv("gender_submission.csv")

row.names(train.df)<-train.df[,1]
row.names(valid.df)<-valid.df[,1]
#View(train.df)
train.df<-train.df[,-1]
valid.df<-valid.df[,-1]
#analyzing each category of the data
variables.df<-data.frame(as.array(summary(train.df$Survived)))
pclass.char<-as.array(summary(train.df$Pclass))
variables.df<-cbind(variables.df,pclass.char)
sex.char<-as.array(summary(train.df$Sex))
#cbind(variables.df,sex.char)
age.char<-as.array(summary(train.df$Age))

sibsp.char<-as.array(summary(train.df$SibSp))
variables.df<-cbind(variables.df,sibsp.char)
parch.char<-as.array(summary(train.df$Parch))
variables.df<-cbind(variables.df,parch.char)
fare.char<-as.array(summary(train.df$Fare))
variables.df<-cbind(variables.df,fare.char)
embarked.char<-as.array(summary(train.df$Embarked))

variables.df<-variables.df[,-c(3,5,7,9)]
colnames(variables.df)<-c("Metric","Survived","Pclass","SibSp","Parch","Fare")
View(variables.df)
View(as.array(summary(train.df$Age)))
View(as.array(summary(train.df$Embarked)))
View(as.array(summary(train.df$Sex)))
#write.csv(variables.df, file = "variables.csv")
#write.csv(sex.char, file = "sexchar.csv")
#write.csv(embarked.char, file = "embarked.csv")
#write.csv(age.char, file = "age.csv")

#calculate the number of people that survived and the average of survival in the training dataset
t.surv<-dim(train.df[train.df$Survived==1,])[1]
t.surv
avg.surv<-t.surv/dim(train.df)[1]
avg.surv
#maximum number of sibling/spouses, parent/children, age and fare in training dataset
as.integer(train.df$Age) #the age of many of the people is unknown

max(train.df$SibSp)
max(train.df$Parch)
max(train.df$Fare)
min(train.df$Fare)#apparently some of the people had their ticket for free

#there are two people whose embarkation port is unknown
train.df[train.df$Embarked=="",]
#these 2 people will be taken out of the dataset
rownames(train.df[train.df$Embarked=="",])
#these 2 people will be taken out of the dataset for graph purposes
trainport.df<-train.df[c(-62,-830),]

#the entries of the people whose age is unknow are
unk.age<-as.integer(rownames(train.df[is.na(train.df$Age),]))
trainage.df<-train.df[-unk.age,] #trainage is train without empty entries
#create age groups (0-10,11-20,...,81-90) for both the train and the trainage datasets
trainage1.df<-trainage.df
trainage1.df$Age<-ceiling(trainage1.df$Age/10)*10
#maximum age
max(trainage.df$Age)

#calculation of the fare ranges
trainfare.df<-train.df
trainfare.df$Fare<-ceiling(trainfare.df$Fare/100)*100


#
#
#
#calculate the average survival of each group
vs.class.df<-aggregate(train.df$Survived,by=list(train.df$Pclass),mean,rm.na=T)
vs.sex.df<-aggregate(train.df$Survived,by=list(train.df$Sex),mean,rm.na=T)
vs.age.df<-aggregate(trainage1.df$Survived,by=list(trainage1.df$Age),mean,rm.na=T)
vs.sibsp.df<-aggregate(train.df$Survived,by=list(train.df$SibSp),mean,rm.na=T)
vs.parch.df<-aggregate(train.df$Survived,by=list(train.df$Parch),mean,rm.na=T)
vs.fare.df<-aggregate(trainfare.df$Survived,by=list(trainfare.df$Fare),mean,rm.na=T)
vs.port.df<-aggregate(trainport.df$Survived,by=list(trainport.df$Embarked),mean,rm.na=T)

#bar charts comparing the the probabilities according to the predictors
par(mfrow=c(3,2))

barplot(vs.class.df[,2], xlab='Ticket Class',ylab='Average Survival',names.arg = c("1st class","2nd class","3rd class"))
abline(h=avg.surv)
barplot(vs.sex.df[,2], xlab='Sex',ylab='Average Survival',names.arg = c("Female","Male"))
abline(h=avg.surv)
barplot(vs.age.df[,2], xlab='Age',ylab='Average Survival'
        ,names.arg=c("0-10","11-20","21-30","31-40","41-50","51-60","61-70","71-80"))
abline(h=avg.surv)
barplot(vs.sibsp.df[,2], xlab='# of siblings/spouses',ylab='Average Survival'
        ,names.arg = c("0","1","2","3","4","5","8"))
abline(h=avg.surv)
barplot(vs.parch.df[,2], xlab='#parents/children',ylab='Average Survival',names.arg = c("0","1","2","3","4","5","6"))
abline(h=avg.surv)
barplot(vs.fare.df[,2], xlab='Fare',ylab='Average Survival',names.arg = c("Free","1-100","101-200","201-300","500+"))
abline(h=avg.surv)
par(mfrow=c(1,1))
barplot(vs.port.df[,2], xlab='Port of Embarkation',ylab='Average Survival',names.arg = c("Chebourg","Queenstown","Southampton"))
abline(h=avg.surv)
#
#
#
