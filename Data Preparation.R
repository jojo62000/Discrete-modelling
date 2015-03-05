#Importing the data into RStudio
train<-read.table("D:/Self Learning/sigmoid/data/train.data/train.data/train.data",sep='\t',header=TRUE)
train_label<-read.table("D:/Self Learning/sigmoid/data/train.data/train.data/train_churn.labels.txt",sep='\t',header=FALSE)
colnames(train_label)<-"flag"

train_label$flag1<-as.numeric(train_label$flag)
train_label$flag1<-ifelse(train_label$flag1==1,1,0)
train1<-cbind(train,train_label[,2])
colnames(train1)<-c(as.character(colnames(train)),"flag")
train<-train1



#checking how many variables have NA's and how many
op1<-data.frame(apply(train,2,function(x) length(which(is.na(x)==TRUE))/length(x)))
perc_na<-op1[1:231,]
variable<-colnames(train)
data<-data.frame("variable"=variable,"perc_na"=perc_na)
#data<-data[order(data$perc_na),]
data<-data[which(data$perc_na<=0.15),]
nrow(data)

#approxmately 76 variables exist where we have more than 85% of the data
#We directly discard the rest, as we cannot use the variables with extremely high missing values

#For the exiting 76 variables, we need to do missing value treatment
#We can use the following steps,
# replace missing values by mean of the column or remove them
#Here for simplicity we remove them (rows)

#Removing the columns which have at least 85% of values in place
sub_train<-train[,as.character(data$variable)]
apply(sub_train,2,class)

#removing all missing values in the dataframe
completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}
n<-completeFun(sub_train,colnames(sub_train))


#Variables 1 - 38 are numeric
#Casting them from char to numeric

a<-data.frame(apply(n[1:38],2,function(x) x<-as.numeric(x)))
summary(a)

b<-data.frame(apply(n[39:76],2,function(x) length(unique(x))))
b
dim(b)
d<-data.frame("a"=colnames(n[39:76]),"b"=b[,1])
d<-d[which(d$b<=10),]
nrow(d)

model1<-n[,as.character(d$a)] #Choosing all the categorical variables with less than 10 values
model1<-data.frame(apply(model1,2,function(x) x<-as.factor(x)))


model2<-n[,1:38]
model<-cbind(model1,model2,n$flag)
colnames(model)<-c(colnames(model1),colnames(model2),"flag")


#Setting a 70:30 ratio 
sample<-sample(1:nrow(model),25000)
train_data<-model[sample,]
test_data<-model[-sample,]
nrow(test_data)

library(e1071)
library(stringr)
library(dplyr)
library(caret)
library(ROCR)
library(e1071)


#Taking a  balanced sample for training
balanced = function(df, group, size) {
  require(sampling)
  temp = df[order(df[group]),]
  if (size < 1) {
    size = ceiling(table(temp[group]) * size)
  } else if (size >= 1) {
    size = rep(size, times=length(table(temp[group])))
  }  
  strat = strata(temp, stratanames = names(temp[group]), 
                 size = size, method = "srswor")
  (dsample = getdata(temp, strat))
}

balanced_train<-balanced(train_data,57,1800)

library(randomForest)

predictors<-colnames(balanced_train)[1:56]

bestmtry <- tuneRF(
                  balanced_train[,1:56],as.factor(balanced_train$flag), 
                  ntreeTry=100, stepFactor=1.5,improve=0.01, trace=TRUE, 
                  plot=TRUE, dobest=FALSE,na.omit=TRUE)


#Dry run with all predictors together
rf<-randomForest(as.factor(flag)~ 
                   Var191 + Var194 + Var196 + Var201 + Var203 + Var205 
                + Var208 + Var210 + Var211 + Var213 + Var215
                + Var218 + Var221 + Var223 + Var224 + Var225 
                + Var227 + Var229 + Var6 + Var7 + Var13 + Var21 
                + Var22 +  + Var24 + Var25 + Var28 + Var35+ Var38 
                + Var44 + Var57 + Var65 +Var73 + Var74
                + Var76 + Var78 + Var81 + Var83 + Var85 + Var109
                + Var112 + Var113 + Var119 + Var123 + Var125
                + Var132 + Var133 + Var134 + Var140 + Var143
                + Var144 + Var149 + Var153 + Var160 + Var163
                + Var173 + Var181 
                ,
                mtry = 7,data=balanced_train,
                   ntree=100,importance=TRUE,replace=TRUE);



