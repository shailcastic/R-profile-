#SVM with different kernels
#Build support vector machines

library(e1071)
data(iris)

summary(iris)
str(iris)

plot(iris$Sepal.Length, iris$Sepal.Width, col=iris$Species)
plot(iris$Petal.Length, iris$Petal.Width, col=iris$Species)

#Build a training and testing set
set.seed(4)
id<-sample(2,nrow(iris),prob = c(0.7,0.3),replace = T)
trainset<-iris[id==1,]
testset<-iris[id==2,]
class(trainset)

#kernel can be linear/polynomial/radial or sigmoid
#cost called as c value - determine the width of the margin, larger the c value, smaller the width
#scale for normalization to avoid bias

model<-svm(Species~. ,data = trainset, kernel = "linear", cost = 0.1)
summary(model)
plot(model, trainset,Petal.Width ~ Petal.Length)
plot(model, trainset,Sepal.Width ~ Sepal.Length)

predvalues<-predict(model,newdata = testset,type = "class")
predvalues

library(caret)
confusionMatrix(table(predvalues, testset$Species))

#Tune for best cost function

set.seed(1)
obj <- tune(svm, Species~., data = trainset, 
            ranges = list(gamma = 2^(-1:1), cost= c(0.001,0.01,0.1,1,5,10,100)),
            tunecontrol = tune.control(sampling = "fix"), kernel='linear')
summary(obj)
best.model <- obj$best.model
summary(best.model)

predvalues_best <-predict(best.model,newdata = testset,type = "class")
predvalues
confusionMatrix(table(predvalues_best, testset$Species))

#Let's change the kernel scenarios

model_poly<-svm(Species~. ,data = trainset, kernel = "polynomial", cost = 0.1)
summary(model_poly)

plot(model_poly, trainset,Petal.Width ~ Petal.Length)

predvalues<-predict(model_poly,newdata = testset,type = "class")
predvalues
confusionMatrix(table(predvalues, testset$Species))

set.seed(1)
obj <- tune(svm, Species~., data = trainset, 
            ranges = list(gamma = 2^(-1:1), cost= c(0.001,0.01,0.1,1,5,10,100)),
            tunecontrol = tune.control(sampling = "fix"), kernel='polynomial')
summary(obj)
best.model <- obj$best.model
summary(best.model)

predvalues_best <-predict(best.model,newdata = testset,type = "class")
predvalues
confusionMatrix(table(predvalues_best, testset$Species))

