install.packages("tm")
install.packages("dplyr")
install.packages("SnowballC")

library("tm")
library("dplyr")
library("SnowballC")

bookHP6 <- readLines("C:/Personal/Module 8/HP6 - The Half Blood Prince.txt")
bookHP7 <- readLines("C:/Personal/Module 8/HP7 - Deathly Hollows.txt")

#To break into words we would require Corpus
CorpusHP6 <- Corpus(VectorSource(bookHP6))%>%
  tm_map(removePunctuation)%>%
  tm_map(removeNumbers)%>%
  tm_map(content_transformer(tolower))%>%
  tm_map(removeWords,stopwords("english"))%>%
  tm_map(stripWhitespace)
  #tm_map(stemDocument)
  #tm_map(removeWords, c("harri", "said", "hermion", "ron", "dumbledoor", "'s")) # Text stemming

#Document term matrix
dtmHP6 <- DocumentTermMatrix(CorpusHP6)
dtmHP6
inspect(dtmHP6[1:10,1:10])

#to remove sparsity
dtmHP6 <- removeSparseTerms(dtmHP6,0.99)
dtmHP6
inspect(dtmHP6[1:10,1:10])

#calculating word frequencies
word.freqHP6 <- sort(colSums(as.matrix(dtmHP6)),decreasing = T)
head(word.freqHP6)

#to get relative frequencies
tableHP6 <- data.frame(word=names(word.freqHP6),absolute.frequency=word.freqHP6,relative.frequency = word.freqHP6/length(word.freqHP6))
head(tableHP6)
rownames(tableHP6)<- NULL
head(tableHP6)
write.csv(tableHP6[1:1000,],"HP6_1000.csv")  

#Let's start with next dataset

bookHP7 <- readLines("C:/Personal/Module 8/HP7 - Deathly Hollows.txt")

#To break into words we would require Corpus
CorpusHP7 <- Corpus(VectorSource(bookHP7))%>%
  tm_map(removePunctuation)%>% # Remove punctuations
  tm_map(removeNumbers)%>% # Remove numbers
  tm_map(content_transformer(tolower))%>% # Convert the text to lower case
  tm_map(removeWords,stopwords("english"))%>% # Remove english common stopwords
  tm_map(stripWhitespace) # Eliminate extra white spaces
  #tm_map(stemDocument)%>% # Text stemming
  #tm_map(removeWords, c("harri", "said", "hermion", "ron", "dumbledoor", "'s")) # Text stemming
# specify your stopwords as a character vector
#CorpusHP7 <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 

#Document term matrix
dtmHP7 <- DocumentTermMatrix(CorpusHP7)
dtmHP7
inspect(dtmHP7[1:10,1:10])

#to remove sparsity
dtmHP7 <- removeSparseTerms(dtmHP7,0.99)
dtmHP7
inspect(dtmHP7[1:10,1:10])

#calculating word frequencies
word.freqHP7 <- sort(colSums(as.matrix(dtmHP7)),decreasing = T)
head(word.freqHP7)

#to get relative frequencies
tableHP7 <- data.frame(word=names(word.freqHP7),absolute.frequency=word.freqHP7,relative.frequency = word.freqHP7/length(word.freqHP7))
head(tableHP7)
rownames(tableHP7)<- NULL
head(tableHP7)
write.csv(tableHP7[1:1000,],"HP7_1000.csv")  

#Merging both the datasets and for two books comparison

finaltable <- tableHP6 %>%
  merge(tableHP7,by="word")%>%
  mutate(dProp=relative.frequency.x-relative.frequency.y,dAbs=abs(dProp))%>%
  arrange(desc(dAbs))%>%
  rename(HP6.freq=absolute.frequency.x, HP6.prop = relative.frequency.x,
         HP7.freq=absolute.frequency.y, HP7.prop = relative.frequency.y)

head(finaltable)

#More visualizations

dtm <- TermDocumentMatrix(CorpusHP7)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

#Generate word cloud

install.packages("wordcloud")
library(wordcloud)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 10,
          max.words=400, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

findFreqTerms(dtm, lowfreq = 4)

barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")

#Bigram cloud generations

install.packages("RWeka")
library(RWeka)

# Bigrams 
minfreq_bigram<-2
token_delim <- " \\t\\r\\n.!?,;\"()"
bitoken <- NGramTokenizer(CorpusHP6, Weka_control(min=2,max=2, delimiters = token_delim))
two_word <- data.frame(table(bitoken))
sort_two <- two_word[order(two_word$Freq,decreasing=TRUE),]
wordcloud(sort_two$bitoken,sort_two$Freq,random.order=FALSE,min.freq = minfreq_bigram,scale = c(1.4,0.7),colors = brewer.pal(8,"Dark2"),max.words=150)

# Trigrams 

minfreq_trigram <- 5
token_delim <- " \\t\\r\\n.!?,;\"()"
tritoken <- NGramTokenizer(CorpusHP6, Weka_control(min=3,max=3, delimiters = token_delim))
three_word <- data.frame(table(tritoken))
sort_three <- three_word[order(three_word$Freq,decreasing=TRUE),]
wordcloud(sort_three$tritoken, sort_three$Freq, random.order=FALSE,min.freq = minfreq_trigram,scale = c(1.4,0.5),colors = brewer.pal(8,"Dark2"),max.words=150)

minfreq_trigram <- 2
token_delim <- " \\t\\r\\n.!?,;\"()"
tritoken <- NGramTokenizer(CorpusHP6, Weka_control(min=5,max=5, delimiters = token_delim))
three_word <- data.frame(table(tritoken))
sort_three <- three_word[order(three_word$Freq,decreasing=TRUE),]
wordcloud(sort_three$tritoken, sort_three$Freq, random.order=FALSE,min.freq = minfreq_trigram,scale = c(1.4,0.5),colors = brewer.pal(8,"Dark2"),max.words=150)


#Sentiment analysis

install.packages("twitteR")
library(twitteR)

consumer_key <- 
consumer_secret <- 
access_token <- 
access_secret <- 

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

#load 1000 tweets of barcelona and menchester

barcelona_tweets <- searchTwitter("FC Barcelona", n=1000, lang="en")
Manchester_tweets <- searchTwitter("Manchester United", n=1000, lang="en")

#Extract text from the tweets

barcelona.text <- lapply(barcelona_tweets,function(x) x$getText())
manchester.text <- lapply(Manchester_tweets,function(x) x$getText())
class(barcelona.text)
barcelona.text[[1]]

#scan to load positive and negative words data into R

pos = scan('positive-words.txt', what='character',comment.char = ";")
neg = scan('negative-words.txt', what='character',comment.char = ";")

#let's create the function for analyzing text

sentence <- barcelona.text

install.packages("stringr")
library(stringr)
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr);
  require(stringr);
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    sentence = gsub('[^A-z ]','', sentence)
    sentence = tolower(sentence);
    word.list = str_split(sentence, '\\s+');
    words = unlist(word.list);
    pos.matches = match(words, pos.words);
    neg.matches = match(words, neg.words);
    pos.matches = !is.na(pos.matches);
    neg.matches = !is.na(neg.matches);
    score = sum(pos.matches) - sum(neg.matches);
    return(score);
  }, pos.words, neg.words, .progress=.progress );
  scores.df = data.frame(score=scores, text=sentences);
  return(scores.df);
}

#Using above function let's calculate the score of FCB tweets
analysisFCB = score.sentiment(barcelona.text, pos,neg,.progress='none')
analysisFCB <- as.data.frame(analysisFCB)
table(analysisFCB$score)
mean(analysisFCB$score)
median(analysisFCB$score)
hist(analysisFCB$score)

analysisMU <- score.sentiment(manchester.text, pos,neg,.progress='none')
table(analysisMU$score)
mean(analysisMU$score)
median(analysisMU$score)
hist(analysisMU$score)

#comparing sentiment score
plot1 <- hist(analysisFCB$score)
plot2 <- hist(analysisMU$score)
plot(plot1, col=rgb(1,0,0,0.25),main="FCB vs MU", xlab = "scores") #Brown
plot(plot2, col=rgb(0,1,0,0.25),add = T) #Brown

#MU has more positive tweets as compared to Barcelona

#Will you like to see more tweets say trump

#load 5000 tweets of Donald trump

covid_tweets <- searchTwitter("covid 19", n=2000, lang="en")

#Extract text from the tweets

covid.text <- lapply(covid_tweets,function(x) x$getText())
class(covid.text)
covid.text[[1]]

#Using above function let's calculate the score of FCB tweets
analysiscovid = score.sentiment(covid.text, pos,neg,.progress='none')
analysiscovid <- as.data.frame(analysiscovid)
table(analysiscovid$score)
mean(analysiscovid$score)
median(analysiscovid$score)
hist(analysiscovid$score)

#CAA
trump_tweets <- searchTwitter("Donald trump", n=2000, lang="en")

#Extract text from the tweets

trump.text <- lapply(trump_tweets,function(x) x$getText())
class(trump.text)
trump.text[[1]]

#Using above function let's calculate the score of FCB tweets
analysistrump = score.sentiment(trump.text, pos,neg,.progress='none')
analysistrump <- as.data.frame(analysistrump)
table(analysistrump$score)
mean(analysistrump$score)
median(analysistrump$score)
hist(analysistrump$score)













  
  
  
  

