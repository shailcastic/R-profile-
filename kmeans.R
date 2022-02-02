#svm
install.packages("readxl")
library(readxl)
data=read_excel("C:\\Users\\SHAILESH TIWARI\\Documents\\Assignment 3 R\\InsuranceData.xlsx")
str(data)
summary(data)


##K-means clustering

#Group the movies to clusters on basis of facebook likes

mov <- read.csv("movie_metadata.csv", header=TRUE)\
View(mov)
mov <- na.omit(mov)
mov$movie_title <- gsub(pattern = " ", replacement = "", mov$movie_title)
colnames(mov)
facebook <- mov[,c(5,6,8,14,25)] #take facebook like column #slicing data from data

#facebook_normalized <- scale(facebook)
km <- kmeans(facebook, 5) #apply k means clustering
km
