#Manipulating the data
backup<-model
data5<-model

#Function to treat outliers in continuos predictors
outlier_treatment<-function(vector)
{
  max_val<-quantile(vector,0.98)
  vector<-ifelse(vector>=max_val,max_val,vector)
}

#Function to treat categorical predictors with skewed levels
category_treatment<-function(factor,string)
{
  factor<-as.factor(ifelse(factor==string,"Yes","No"))
}

#Function to create a confusion matrix
accuracy_check<-function(table)
{ a<-matrix(nrow=3,ncol=3)
  a[1,1]<-table[1]
  a[1,2]<-table[3]
  a[1,3]<-table[1]/(table[1]+table[3])
  a[2,1]<-table[2]
  a[2,2]<-table[4]
  a[2,3]<-table[4]/(table[2]+table[4])
  a[3,1]<-table[1]/(table[1]+table[2])
  a[3,2]<-table[4]/(table[3]+table[4])
  a[3,3]<-(table[1]+table[4])/sum(table)
  return(a)
}


#Treating Skewed categorical variables
data5$Var194<-category_treatment(data5$Var194,"")
data5$Var203<-category_treatment(data5$Var203,"9_Y1")
data5$Var205<-category_treatment(data5$Var205,"VpdQ")
data5$Var210<-category_treatment(data5$Var210,"uKAI")

#The variables which have outliers are treated by 
#replacing all values above 98th percentile by the value of 98th percentile
data5$Var6<-outlier_treatment(data5$Var6)
data5$Var7<-outlier_treatment(data5$Var7)
data5$Var13<-outlier_treatment(data5$Var13)
data5$Var21<-outlier_treatment(data5$Var21)
data5$Var25<-outlier_treatment(data5$Var25)
data5$Var38<-outlier_treatment(data5$Var38)
data5$Var65<-outlier_treatment(data5$Var65)
data5$Var74<-outlier_treatment(data5$Var74)
data5$Var81<-outlier_treatment(data5$Var81)
data5$Var83<-outlier_treatment(data5$Var83)
data5$Var119<-outlier_treatment(data5$Var119)
data5$Var134<-outlier_treatment(data5$Var134)
data5$Var144<-outlier_treatment(data5$Var144)
data5$Var160<-outlier_treatment(data5$Var160)



#Splitting into 90:10
sample<-sample(1:nrow(data5),round(nrow(data5)*0.9))
train_data<-data5[sample,]
test_data<-data5[-sample,]
nrow(test_data)

#Storing the number of 1's to get a balanced sample for training
no_of_ones<-as.numeric(summary(as.factor(train_data$flag))["1"])
balanced_train<-balanced(train_data,57,no_of_ones)

#----------Random Forest Iteration-----------------------------------------
rf2<-randomForest(as.factor(flag)~ 
                    Var74 + Var218 +Var73 + Var225 + Var229 # + Var21
                  + Var210 + Var22 + Var6 + Var13 + Var144 + Var81 + Var85 
                  + Var83 + Var65 + Var211 + Var7 + Var113 
                  + Var125  + Var160 + Var134 + Var25 + Var119 
                  + Var57
                  ,
                  mtry = 4,data=balanced_train,
                  ntree=300,importance=TRUE,replace=TRUE);

rf2
varImpPlot(rf2)

#Testing for accuracy
test1<-test_data
test1$predict <- predict(rf2,newdata=test1)
k<-as.data.frame.matrix(table(test1$predict,test1$flag))
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
varUsed(rf2)


#----------------Logistic Regression Iteration--------------------------
lr<-glm(as.factor(flag)~ 
          Var74 + Var218 +Var73 + Var225 + Var229 
        #+ Var21
        + Var210 + Var22 + Var6 + Var13 + Var144 + Var81 + Var85 
        + Var83 + Var65 + Var211 + Var7 + Var113 
        + Var125  + Var160 + Var134 + Var25 + Var119 
        + Var57        ,
        data=balanced_train,
        family="binomial")
summary(lr)
vif(lr)
predicted<-predict(lr,newdata=test_data,type="response")
actual<-test_data$flag
predicted_values<-ifelse(predicted<=0.5,0,1)
a<-table(predicted_values,actual)
accuracy<-accuracy_check(a)
accuracy

#------------Support Vector Machine Iterations---------------------
model<-svm(as.factor(flag)~ 
             Var74 + Var218 +Var73 + Var225 + Var229
           #+ Var21
           + Var210 + Var22 + Var6 + Var13 + Var144 + Var81 + Var85 
           + Var83 + Var65 + Var211 + Var7 + Var113 
           + Var125  + Var160 + Var134 + Var25 + Var119 
           + Var57        ,
           data=balanced_train,
           kernel="radial",
           scale=3,
           gamma=0.3,cost=5,
           degree=5,
           epsilon=0.1,
           type="C-classification" )

#summary(model)
print(model)
#Scoring the model to check the accuracy


actual<-test_data$flag
predicted_svm<-predict(model,newdata=test_data,outcome="actual")
a<-table(predicted_svm,actual)
accuracy<-accuracy_check(a)
accuracy