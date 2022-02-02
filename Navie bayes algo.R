#Build naive bayes model
setwd("C:\\Users\\SHAILESH TIWARI\\Documents\\Assignment 3 R")
diabetic<-read.csv("Diabetes.csv")

summary(diabetic)
str(diabetic)
library(e1071)

diabetic$Is_Diabetic<-as.factor(diabetic$Is_Diabetic)
View(diabetic)
#Build a training and testing set
set.seed(4)
id<-sample(2,nrow(diabetic),prob = c(0.7,0.3),replace = T)
trainset<-diabetic[id==1,]
testset<-diabetic[id==2,]
class(trainset)

model<-naiveBayes(trainset$Is_Diabetic~. ,data = trainset)
model

predvalues<-predict(model,newdata = testset,type = "class")
predvalues

library(caret)

confusionMatrix(table(predvalues, testset$Is_Diabetic))
??confusionMatrix
