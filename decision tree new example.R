#decision tree New Example solved full
library(datasets)
library(caTools)
#install.packages("party")
library(party)
library(dplyr)
data("readingSkills")
head(readingSkills)
summary(readingSkills)
str(readingSkills)


sample_data=sample.split(readingSkills,SplitRatio = 0.8)
train_data= subset(readingSkills,sample_data= TRUE)
test_data= subset(readingSkills,sample_data= FALSE)

model=ctree(nativeSpeaker~ .,train_data)

plot(model)


predict_model=predict(ctree(formula(data)),test_data)
