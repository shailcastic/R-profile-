getwd()
setwd("C:\\Users\\SHAILESH TIWARI\\Documents\\Assignment 3 R")

library(recommenderlab)
library(reshape2)
library(arules)
library(dplyr)
library(Matrix)
library(caret)

movies = read.csv("movie.csv",stringsAsFactors = FALSE)
ratings=read.csv("rating.csv")
str(movie_data)
class(movie_data)
summary(movie_data)
head(movie_data)
View(movie_data)

rating_df<-ratings
 
ratingmat<-dcast(rating_df,userId~movieId,value.var = "rating",na.rm=FALSE)
ratingmat<-as.matrix(ratingmat[,-1])
ratingmat<-as(ratingmat,"realRatingMatrix")
#normaliza the data
ratingmat_norm<-normalize(ratingmat)
#building a model
recommender_model<-Recommender(ratingmat_norm,method="UBCF",param=list(method="Cosine",nn=30))
recom<-predict(recommender_model,ratingmat[1],n=10) # obtaining top 10 records
recom_list<-as(recom,"list") #convert recommenderlab object to readable list

#obtaining recommendation
recom_result<-matrix(0,10)
for(i in c(1:10)){
  recom_result[i]<-movies[as.integer(recom_list[[1]][i]),2]
}
recom_result


#evaluation
evaluation_scheme<- evaluationScheme(ratingmat,method="cross-validation",k=5,given=2,goodRating=5)
evaluation_scheme

evaluation_results<-evaluate(evaluation_scheme,method = "UBCF",n=c(1,3,5,10,15,20))
print(evaluation_results)

eval_results<-getConfusionMatrix(evaluation_results)[[1]]
eval_results
}

