library(tm)
library(wordcloud)
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(dplyr)

reviews <- read.csv(file.choose(), header = T)

str(reviews)

#creating a Corpus
corpus <- iconv(reviews$Body)
corpus <- Corpus(VectorSource(corpus))

inspect(corpus[1:5])

#cleaning the corpus
corpus <- tm_map(corpus, tolower)

corpus <- tm_map(corpus, removePunctuation)

corpus <- tm_map(corpus, removeNumbers)

corpus <- tm_map(corpus, removeWords, stopwords("english"))

corpus <- tm_map(corpus, stripWhitespace)
inspect(corpus[1:5])

corpus <- tm_map(corpus, removeWords, c("ready","player", "book","books","read"))

review_final <- corpus

#Create Term Matrix
tdm <- TermDocumentMatrix(review_final)
tdm <- as.matrix(tdm)
tdm[1:10,1:5]

#barplot
w <- rowSums(tdm)
w <- subset(w, w>=10)
barplot(w, las =2 , col = rainbow(50))

#wordcloud
w <- sort(rowSums(tdm), decreasing = TRUE)
set.seed(2000)
wordcloud(words = names(w),
                  freq = w,
                  max.words = 50,
                  random.order = F,
                  min.freq = 10,
                  colors = brewer.pal(3, 'Dark2'),
                  scale = c(3.0,0.3))
library(reshape2)

#obtain Sentiment Scores
review <- read.csv(file.choose(), header = T)
cor <- iconv(review$Body)
s <- get_nrc_sentiment(cor)
head(s)

#Bar Plot
barplot(colSums(s), las = 2, col = rainbow(10), ylab = 'Count', main = 'Sentiment score for reviews')
