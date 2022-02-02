getwd()
data=read.csv("movie_metadata.csv")
summary(data)
str(data)
#data visualization
stripchart(data$gross)
hist(data$gross)
hist(data$title_year)
boxplot(data$imdb_score)
plot(data$title_year,data$director_facebook_likes)
summary(data$imdb_score)
data[data$imdb_score>9,]#condition to remove outliers
 #transformation of features
mtcars=read.csv("mtcars.csv")
plot(hist(mtcars$disp))  #for skewness in data
summary(mtcars$disp)

plot(hist(log(mtcars$disp)))
summary(log(mtcars$disp))  #log transformation

plot(hist(sqrt(mtcars$disp)))   #sqrt transformation
summary(sqrt(mtcars$disp))

plot(hist(exp(mtcars$disp)))
summary(exp(mtcars$disp)) #exponential distribution


hist(data$imdb_score)
plot(hist(log(data$imdb_score)))

#multiple histogram on one plot
install.packages("gridExtra")
library(gridExtra)

a=qplot(x=mtcars$disp,data=mtcars)
b=qplot(x=log(mtcars$disp),data=mtcars)
c=qplot(x=sqrt(mtcars$disp),data=mtcars)

grid.arrange(a,b,c,ncol(1))


data("iris")
iris
hist(iris$Sepal.Length)

iris_1=iris[iris$Species=='setosa',]
hist(iris_1$Sepal.Length)
summary(iris_1$Sepal.Length)
hist(iris_1$Sepal.Width)
hist(iris_1$Petal.Length)
hist(iris$Petal.Length)
hist(log(iris$Petal.Length))
 install.packages("mlbench")
library(mlbench) 
dataset=read.csv("Boston Housing.csv")
str(dataset)
summary(dataset)
table(is.na(dataset))
#introducin missing values in the dataset
dataset[sample(1:nrow(dataset),40),"rad"]
dataset[sample(1:nrow(dataset),40),"ptratio"]

#imputation with mean median and mode

install.packages("Hmisc")
install.packages("ggplot2")
library(Hmisc)
dataset$onsetdate=NULL #removing column
