library(lubridate)
library(dplyr)
library(reshape2)
data<-read.csv("C:/Users/Jojo John M/Desktop/Model/Renewal_Prediction.csv")


glimpse(data)
aggregate(C14~Renewal, data=data,length)
head(data[,1])

temp<-as.data.frame(aggregate(data$C1,by=list(data$Renewal,data$C2), length))
colnames(temp)<- c("renewal","c1","count")
temp2<-dcast(renewal~c1,data=temp,value.var="count",sum)


head(as.numeric(as.character(data$C2)))
head(((data$C2)))
quantile(as.numeric(as.character(data$C2)),seq(0.1,1.0,0.1),na.rm=TRUE)
quantile(as.numeric(as.character(data$C3)),seq(0.1,1.0,0.1),na.rm=TRUE)
length(data$C1[data$C1 == '?'])

t(apply(data,2,function(x){length(which(x=='?'))}))



temp<-as.data.frame(aggregate(data$C1,by=list(data$Renewal,data$C4), length))
colnames(temp)<- c("renewal","c1","count")
temp2<-dcast(renewal~c1,data=temp,value.var="count",sum)



temp<-as.data.frame(aggregate(data$C1,by=list(data$Renewal,data$C5), length))
colnames(temp)<- c("renewal","c1","count")
temp2<-dcast(renewal~c1,data=temp,value.var="count",sum)


temp<-as.data.frame(aggregate(data$C1,by=list(data$Renewal,data$C6), length))
colnames(temp)<- c("renewal","c1","count")
temp2<-dcast(renewal~c1,data=temp,value.var="count",sum)


temp<-as.data.frame(aggregate(data$C1,by=list(data$Renewal,data$C7), length))
colnames(temp)<- c("renewal","c1","count")
temp2<-dcast(renewal~c1,data=temp,value.var="count",sum)


quantile(data$C8, c(seq(0.1,1.0,0.1),0.95,0.99))
quantile(data$C8, c(0.0,0.25,0.5,0.75,1.0))



temp<-as.data.frame(aggregate(data$C1,by=list(data$Renewal,data$C9), length))
colnames(temp)<- c("renewal","c1","count")
temp2<-dcast(renewal~c1,data=temp,value.var="count",sum)

temp<-as.data.frame(aggregate(data$C1,by=list(data$Renewal,data$C10), length))
colnames(temp)<- c("renewal","c1","count")
temp2<-dcast(renewal~c1,data=temp,value.var="count",sum)


quantile(data$C11, c(seq(0.1,1.0,0.1),0.95,0.99))


temp<-as.data.frame(aggregate(data$C1,by=list(data$Renewal,data$C12), length))
colnames(temp)<- c("renewal","c1","count")
temp2<-dcast(renewal~c1,data=temp,value.var="count",sum)


temp<-as.data.frame(aggregate(data$C1,by=list(data$Renewal,data$C13), length))
colnames(temp)<- c("renewal","c1","count")
temp2<-dcast(renewal~c1,data=temp,value.var="count",sum)

temp<-as.data.frame(aggregate(data$C1,by=list(data$Renewal,data$C1), length))
colnames(temp)<- c("renewal","c1","count")
temp2<-dcast(renewal~c1,data=temp,value.var="count",sum)


class(data$C15)

quantile(as.numeric(as.character(data$C14)), c(seq(0.1,1.0,0.1),0.95,0.99),na.rm=TRUE)

quantile(as.numeric(as.character(data$C15)), c(seq(0.1,1.0,0.1),0.95,0.99),na.rm=TRUE)

quantile(as.numeric(as.character(data$C2)), c(seq(0.1,0.9,0.1),0.95,0.99,1.0),na.rm=TRUE)

quantile(as.numeric(as.character(data$C3)), c(seq(0.1,0.9,0.1),0.95,0.99,1.0),na.rm=TRUE)



#-----------------------------------------------------------------

#Data cleaning and type conversion


final<-data %>% mutate(
                C2=as.numeric(as.character(C2))  ,  
                C14= as.numeric(as.character(C14)),
                Renewal=as.factor(Renewal)
                )

final <- final %>% filter(
                          C1 != '?',
                          C2 != '?',
                          C4 != '?',
                          C5 != '?',
                          C6 != '?',
                          C7 != '?',
                          C14 != '?'
                          )
                
final<- final %>% filter(C4 != 'l',C5 != 'gg')
final$C3<-ifelse(final$C3>14,14,final$C3)
final$C11_new<-ifelse(final$C11 == 0,0,1)

#Mutating C7
#final<-mutate(final,C7=as.factor(ifelse(C7 %in% c("v","h","bb","ff"),C7,"Others")))



train_index<-sample(1:nrow(final),round(nrow(final)*0.7))
train<-final[train_index,]
test<-final[-train_index,]
library(randomForest)
RF_model<-randomForest(Renewal~
#                C1 +
                C2 +
                C3 +
                C4 +
                C5 +
                C6 +
                C7 +
                C8 +
                C9 +
                C10 +
                C11 +
                C12 +
                C13 +
#                C14 +
                C15 ,
              data=train, mtry=2,ntree=500,importance=TRUE,replace=TRUE)
RF_model
varImpPlot(RF_model)



test$predicted<-predict(RF_model,newdata = test)


k<-as.data.frame.matrix(table(test$predict,test$Renewal))
for(i in 1:2)
{
  k[i,3]<-k[i,i]/sum(k[i,c(1:2)])
}
for(i in 1:2)
{
  k[3,i]<-k[i,i]/sum(k[c(1:2),i])
}
k[3,3]<-(k[1,1]+k[2,2])/sum(k[c(1:2),c(1:2)])
colnames(k)<-c(1,2,"Accuracy")
k


library(ROCR)

ncol_val<-ncol(test)
scr.rf.pr = predict(RF_model,type="prob",newdata=test)[,2]
scr.rf.pred = prediction(scr.rf.pr, test$Renewal)
scr.rf.perf = performance(scr.rf.pred,"tpr","fpr")
auc.area_rdock_val <- slot(performance(scr.rf.pred, "auc") , "y.values")[[1]]
plot(scr.rf.perf,col="blue",lwd=2)

total<-final
par(new=TRUE)
scr.rf.pr = predict(RF_model,type="prob",newdata=total)[,2]
scr.rf.pred = prediction(scr.rf.pr, total$Renewal)
scr.rf.perf = performance(scr.rf.pred,"tpr","fpr")
auc.area_rdock_total <- slot(performance(scr.rf.pred, "auc") , "y.values")[[1]]
plot(scr.rf.perf,col="red",lwd=2)


par(new=TRUE)
scr.rf.pr = predict(RF_model,type="prob",newdata=train)[,2]
scr.rf.pred = prediction(scr.rf.pr, train$Renewal)
scr.rf.perf = performance(scr.rf.pred,"tpr","fpr")
auc.area_rdock_train <- slot(performance(scr.rf.pred, "auc") , "y.values")[[1]]
plot(scr.rf.perf,main="ROC Curve for Random Forest",col="grey",lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray") 

#Adding the legeng to the plot

legend("bottom",  c("Training","Hold Out","Total"), 
       
       lty=c(1,1), 
       
       lwd=c(2.5,2.5),col=c("grey","blue","red"))
varImpPlot(adult.rf)
k
adult.rf
auc.area_rdock_val 
auc.area_rdock_total
auc.area_rdock_train
