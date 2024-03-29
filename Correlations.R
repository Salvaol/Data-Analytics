#data has already been provided in training and validation datasets
train.df<-read.csv("train.csv")
valid.df<-read.csv("test.csv")
row.names(train.df)<-train.df[,1]
row.names(valid.df)<-valid.df[,1]

#View(train.df)
train.df<-train.df[,-1]
valid.df<-valid.df[,-1]

#there are two people whose embarkation port is unknown
train.df[train.df$Embarked=="",]
#these 2 people will be taken out of the dataset
rownames(train.df[train.df$Embarked=="",])
#these 2 people will be taken out of the dataset for graph purposes
trainport.df<-train.df[c(-62,-830),]

#the entries of the people whose age is unknow are
unk.age<-as.integer(rownames(train.df[is.na(train.df$Age),]))
trainage.df<-train.df[-unk.age,] #trainage is train without empty entries

#Some variables don�t provide information
selected.var<-c(1, 2, 4, 5, 6, 7, 9, 11)
trainage.df<-trainage.df[,selected.var]

#The dummy variables are created
trainage.df$Male<-ifelse(trainage.df$Sex=="male",1,0)
trainage.df$Female<-ifelse(trainage.df$Sex=="female",1,0)
trainage.df$Sex<-NULL
trainage.df$Q<-ifelse(trainage.df$Embarked=="Q",1,0)
trainage.df$S<-ifelse(trainage.df$Embarked=="S",1,0)
trainage.df$C<-ifelse(trainage.df$Embarked=="C",1,0)
trainage.df$Embarked<-NULL
trainage.df$C1<-ifelse(trainage.df$Pclass==1,1,0)
trainage.df$C2<-ifelse(trainage.df$Pclass==2,1,0)
trainage.df$C3<-ifelse(trainage.df$Pclass==3,1,0)
trainage.df$Pclass<-NULL
#Correlations heatmap
library(gplots)
heatmap.2(cor(trainage.df), Rowv=FALSE, Colv = FALSE, dendrogram = "none",
          cellnote = round(cor(trainage.df),2),
          notecol = "black", key = FALSE, trace = 'none', margins = c(10,10))

