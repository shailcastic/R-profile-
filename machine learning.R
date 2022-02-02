#module 4 machine learning
#supervised learning:  both input and output variable present as in 
#regression(continuous variable) and classification(class of two(binary logistic regression) 
#or three or many decision tree or random forest)
#unsupervised only input dataset not output dataset kmeans clusstering
data("cars")
summary(cars)
str(cars)
?cars
plot(cars$speed,cars$dist)
cor(cars$speed,cars$dist)
cr=cor(cars)
install.packages("corrplot")
library(corrplot)
corrplot(cr,type='lower')
rec.lm=lm(cars$dist~cars$speed,data=cars)
#cars$distance ~ . #for multiple variable
summary(rec.lm)
distance=
  
#creating train and test
set.seed(100)
trainingRowIndex=sample(1:nrow(cars),