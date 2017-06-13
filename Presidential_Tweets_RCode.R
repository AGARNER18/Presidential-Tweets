# Amber Garner
# Sentiment Analysis: Text Analysis: Twitter Analysis 
# 6/7/2017

install_url("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz")
install.packages("plyr")
source("http://bioconductor.org/biocLite.R")
biocLite("Rgraphviz")
library(DT)
library(stm)
library(topicmodels)
library(wordcloud2)
library('RSentiment')
library("Rgraphviz")
library(sentiment)
library(devtools)
library(ggplot2)
library(cluster)
library(fpc)
library(wordcloud)
library(SnowballC)
library(tm)
library(twitteR)
library(plyr)

# set up twitter access
consumer_key<-#add consumer key
consumer_secret<-#add consumer secret
access_token<-#add access token
access_secret<-#add access secret
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

#********************************************************President Obama**********************************************

# pull tweets for obama presidency
tweets<- userTimeline("BarackObama", n=3200, since="2013-01-20", includeRts = FALSE)

# transform lists to data frames
obama <- do.call("rbind", lapply(tweets, as.data.frame))

# get only the text
some_txt = obama$text

# look at text before cleaning
some_txt[10]

#***STAGE ONE PRE-PROCESSING***

# remove retweet
some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)

# remove at people
some_txt = gsub("@\\w+", "", some_txt)

# remove punctuation
some_txt = gsub("[[:punct:]]", "", some_txt)

# remove numbers
some_txt = gsub("[[:digit:]]", "", some_txt)

# remove html links
some_txt = gsub("http\\w+", "", some_txt)

# remove unnecessary spaces
some_txt = gsub("[ \t]{2,}", "", some_txt)
some_txt = gsub("^\\s+|\\s+$", "", some_txt)

# define "tolower error handling" function 
try.error = function(x)
{
  # create missing value
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error=function(e) e)
  # if not an error
  if (!inherits(try_error, "error"))
    y = tolower(x)
  # result
  return(y)
}
# lower case using try.error with sapply 
some_txt = sapply(some_txt, try.error)

# remove NAs in some_txt
some_txt = some_txt[!is.na(some_txt)]
names(some_txt) = NULL

# look at text after cleaning
some_txt[3]

#***TWEET EMOTION CLASSIFICATION***

# emotion analysis on some_txt
emotion<-classify_emotion(some_txt, algorithm = "bayes", prior=1.0, verbose = TRUE)

# get only the emotion prediction
emotion_2 = emotion[,7]

# convert NA emotions to unkown
emotion_2[is.na(emotion_2)] = "unknown"

#***TWEET POLARITY CLASSIFICATION***

# polarity analysis on some_text
class_polarity= classify_polarity(some_txt, algorithm="bayes", verbose = TRUE)

# get only the polarity prediction
polarity = class_polarity[,4]

# create new data frame with the text, emotion prediction, and polarity prediction
sent_df = data.frame(text=some_txt, emotion=emotion_2, polarity=polarity, stringsAsFactors=FALSE)

# sort new data frame
sent_df = within(sent_df, emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))

# look at emotion and polarity classification of text previously viewed
sent_df[11,]

# plot distribution of emotions with unknown
ggplot(sent_df, aes(x=emotion)) +
  geom_bar(aes(y=..count.., fill=emotion)) +
  ggtitle("Emotions of Obama Tweets") +
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  scale_fill_brewer(palette="RdBu") +
  labs(x="emotion categories", y="number of tweets")

# subset to exclude rows where the emotion is unkown
no_un<-sent_df[sent_df$emotion != "unknown",]

# plot distribution of emotions without unknown
ggplot(no_un, aes(x=emotion)) +
  geom_bar(aes(y=..count.., fill=emotion)) +
  ggtitle("Emotions of Obama Tweets") +
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  scale_fill_brewer(palette="RdBu") +
  labs(x="emotion categories", y="number of tweets")

# plot distribution of polarity 
ggplot(sent_df, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +
  ggtitle("Polarity of Obama Tweets") +
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  scale_fill_brewer(palette="RdBu") +
  labs(x="polarity categories", y="number of tweets")

#***STAGE TWO DATA PRE-PROCESSING***

# convert to corpus for word clouds
all<-Corpus(VectorSource(sent_df$text))

# Look at conversion from df to corpus 
inspect(all[[3]])
sent_df[3,1]

# remove stop words
all<-tm_map(all, removeWords, stopwords("english"))
inspect(all[[5]])

# remove word endings such as ed or s
all<-tm_map(all, stemDocument)
inspect(all[[3]])

# remove anything other than English letters or space
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
all <- tm_map(all, content_transformer(removeNumPunct))

# function to remove special characters
toSpace<-content_transformer(function(x,pattern)gsub(pattern, "", x))
all<-tm_map(all, toSpace, "\\???$")
all<-tm_map(all, toSpace, "\\â$")
all<-tm_map(all, toSpace, "amp")

# Get word frequencies
dtm<-DocumentTermMatrix(all)
dtm

# calculate sum for how many times each term was used
freq<-colSums(as.matrix(dtm))
freq

# find number of distinct terms
length(freq)

#how many times each term appears in each document
m<-as.matrix(dtm)
m

#***TERM SENTIMENT ANALYSIS***
# create document term matrix 
dtm_up = DocumentTermMatrix(VCorpus(VectorSource(all)))
# calcualte frequencies
freq_up <- colSums(as.matrix(dtm_up))
# calculate the sentiment of the terms
sentiments_up = calculate_sentiment(names(freq_up))
# combine the sentiment predictions with the frequencies
sentiments_up = cbind(sentiments_up, as.data.frame(freq_up))
# subset to only include the positive terms in a new variable 
sent_pos_up = sentiments_up[sentiments_up$sentiment == 'Positive',]
# subset to only include the negative terms in a new variable
sent_neg_up = sentiments_up[sentiments_up$sentiment == 'Negative',]

# compare number of positive terms to negative terms
cat("negative terms: ",sum(sent_neg_up$freq_up)," positive terms: ",sum(sent_pos_up$freq_up))

# create word cloud of only the positive words
DT::datatable(sent_pos_up)
positive<-sent_pos_up[,-2]
layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
par(mar=rep(0, 4))
plot.new()
set.seed(100)
wordcloud(positive$text, positive$freq_up, colors = c( "royalblue","royalblue3", "navyblue"), min.freq=1, max.words = 200)
# create word cloud of only the negative words
DT::datatable(sent_neg_up)
negative <-sent_neg_up[,-2]
layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
par(mar=rep(0, 4))
plot.new()
set.seed(100)
wordcloud(negative$text, negative$freq_up, colors = c("red", "darkred", "firebrick2"), min.freq=1,max.words = 200)

# get dimensions of number of documents and the number of distinct terms
dim(m)

# find frequent terms
findFreqTerms(dtm, lowfreq = 2)

# remove sparse terms that appear in less than 95% of documents
dtms<-dtm
dtms

#***CORRELATION***

# find associations between words found in the same document
findAssocs(dtms, c("gun", "fight", "partisan"), corlimit=0.23)

#correlation plot

# build with 15 randomly selected words that appear at least 5 times with a correlation of 0.1 or greater
correlation_plot<-plot(dtm, terms=findFreqTerms(dtm, lowfreq = 2)[1:20], corThreshold = 0.2, weighting=T)

#***FREQUENCY***

# word frequency plot that appears at least 2 times
wf<-data.frame(word=names(freq), freq=freq)
p<-ggplot(subset(wf, freq>15), aes(word, freq))
p<-p+ geom_bar(stat="identity", colour="black", fill="royalblue")
p<-p+ theme(axis.text.x = element_text(angle=45, hjust = 1))
all_freq_plot<-p
all_freq_plot

# crete matrix of frequencies
freq<-colSums(as.matrix(dtm))

# create word cloud of terms 
wordcloud(names(freq), freq, min.freq =3000, max.words=150, color=c("royalblue3","red", "royalblue", "red3", "firebrick2"))


#***CLUSTERING***

# remove the terms that appear less than 95% of documents
dtms<-removeSparseTerms(dtm, 0.95)
# build dissimiliatry matrix and store in d
d<-dist(t(dtms), method = "euclidian")
# use kmeans to build 6 cluster
kfit<-kmeans(d, 6)
# view clusters
kfit
# plot the clusters
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)
# Find cluster details
kfit$cluster

# plot sum of squares distances between cluster and sum of squared distance between clusters for k between 2 and 15
# find optimal level of clusters for kmeans
bss<-integer(length(2:15))
for (i in 2:15) bss[i] <- kmeans(d,centers=i)$betweenss
plot(1:15, bss, type="b", xlab="Number of Clusters",  ylab="Sum of squares", col="blue") 
wss<-integer(length(2:15))
for (i in 2:15) wss[i] <- kmeans(d,centers=i)$tot.withinss
lines(1:15, wss, type="b" )

# build dissimiilarity matrix 
d<-dist(t(dtms), method = "euclidian")
# build dendogram and store in variable fit
fit<-hclust(d=d, method="ward.D2")
# plot dendogram
plot(fit, hang=1)
# add red line to show the cluster each belows to 
plot.new()
plot(fit, hang = -1)
groups<-cutree(fit, k=6)
rect.hclust(fit, k=6, border = "red")

#***LDA***

# make sure each row has at least one term
rowsum.dtm<-apply(dtm, 1, sum)
dtms[rowsum.dtm>0,]

# get the topics using LDA funciton with 5 topics 
lda.obama.notes<-LDA(dtm[rowsum.dtm>0,], k=5, method="Gibbs", control=list(nstart=5, seed=list(101, 102, 103, 104, 105), best=TRUE, burnin=4000, iter=2000, thin=500))
summary(lda.obama.notes)

# get the topic terms 
topics.terms<-terms(lda.obama.notes,5)
# create a table of topic terms
knitr::kable(topics.terms)
# create list seperated by commas of terms
topics.terms.lst<-apply(topics.terms, MARGIN = 2, paste, collapse=", ")
write(topics.terms.lst,"", 40, sep = "\n")
# get only the topic number
topics.obama.notes<-topics(lda.obama.notes, 1)
# get how often a topic term is used
topics.term.freq<-rowSums(as.matrix(dtm[rowsum.dtm>0,]))
# combine the topic number and how often the topic term is used into a data frame
topic.df<-data.frame(topics.term.freq, topics.obama.notes)
# create bar plot showing how often each topic appears in the tweets
barplot(table(topic.df$topics.obama.notes), names.arg = c("Topic-1", "Topic-2", "Topic-3", "Topic-4", "Topic-5"), col = blues9, main = "Obama Tweets Topic By Document", ylab = "Documents")

# density plot of number of terms by number of documents with transparency for better readability
ob<-qplot(topics.term.freq,..count.., data = topic.df, geom = "density", fill=topics.terms.lst[topics.obama.notes], xlab = "Total Terms in Topics", ylab = "Documents Covered By Topics") 
ob<- ob + scale_fill_brewer(palette="RdBu")
ob<-ob+ labs(fill="Topics")
ob<-ob + geom_density(alpha=.4)
ob


#*********************************************************President Trump****************************************************************

# get Trump tweets
tweets2<- userTimeline("realDonaldTrump", n=3200, since="2017-01-20", includeRts = FALSE)

# convert lists to data frame
trump <- do.call("rbind", lapply(tweets2, as.data.frame))

#***STAGE ONE DATA PRE-PROCESSING***

# get the text only 
some_txt = trump$text

# look at text before cleaning
some_txt[3]

# remove retweet 
some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)

# remove at people
some_txt = gsub("@\\w+", "", some_txt)

# remove punctuation
some_txt = gsub("[[:punct:]]", "", some_txt)

# remove numbers
some_txt = gsub("[[:digit:]]", "", some_txt)

# remove html links
some_txt = gsub("http\\w+", "", some_txt)

# remove unnecessary spaces
some_txt = gsub("[ \t]{2,}", "", some_txt)
some_txt = gsub("^\\s+|\\s+$", "", some_txt)

# define "tolower error handling" function 
try.error = function(x)
{
  # create missing value
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error=function(e) e)
  # if not an error
  if (!inherits(try_error, "error"))
    y = tolower(x)
  # result
  return(y)
}
# lower case using try.error with sapply 
some_txt = sapply(some_txt, try.error)

# remove NAs in some_txt
some_txt = some_txt[!is.na(some_txt)]
names(some_txt) = NULL

# look at text after cleaning
some_txt[3]

#***TWEET EMOTION CLASSIFICATON***
# emotion analysis on some_txt
emotion<-classify_emotion(some_txt, algorithm = "bayes", prior=1.0, verbose = TRUE)

# get only the emotion predictions
emotion_2 = emotion[,7]

# change NA emotions to unknown
emotion_2[is.na(emotion_2)] = "unknown"

#***TWEET POLARITY CLASSIFICATION***

# classify polarity
class_polarity= classify_polarity(some_txt, algorithm="bayes", verbose = TRUE)

# get polarity prediction
polarity = class_polarity[,4]

# data frame with text, emotion predictions, and polarity predictions
sent_df = data.frame(text=some_txt, emotion=emotion_2, polarity=polarity, stringsAsFactors=FALSE)

# sort data frame
sent_df = within(sent_df, emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))

# look at emotion and polarity classification of text previously viewed
sent_df[11,]

# plot distribution of emotions with unknown
ggplot(sent_df, aes(x=emotion)) +
  geom_bar(aes(y=..count.., fill=emotion)) +
  ggtitle("Emotions of Trump Tweets") +
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  scale_fill_brewer(palette="RdBu") +
  labs(x="emotion categories", y="number of tweets")

# subset to exclude rows with unknown emotions
no_un<-sent_df[sent_df$emotion != "unknown",]

# plot distribution of emotions without unknown
ggplot(no_un, aes(x=emotion)) +
  geom_bar(aes(y=..count.., fill=emotion)) +
  ggtitle("Emotions of Trump Tweets") +
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  scale_fill_brewer(palette="RdBu") +
  labs(x="emotion categories", y="number of tweets") 
  

# plot distribution of polarity 
ggplot(sent_df, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +
  ggtitle("Polarity of Trump Tweets") +
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  scale_fill_brewer(palette="RdBu") +
  labs(x="polarity categories", y="number of tweets")

#***STAGE TWO DATA PRE-PROCESING***

# convert to corpus for word clouds
all<-Corpus(VectorSource(sent_df$text))

# Look at conversion from df to corpus 
inspect(all[[3]])
sent_df[3,1]


# remove stop words
all<-tm_map(all, removeWords, stopwords("english"))
inspect(all[[5]])

# remove word endings such as ed or s
all<-tm_map(all, stemDocument)
inspect(all[[3]])

# remove url from document text
removeURL<-function(x)gsub("http[^[:space:]]*","",x)
all<-tm_map(all, content_transformer(removeURL))

# remove anything other than English letters or space
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
all <- tm_map(all, content_transformer(removeNumPunct))

# function to remove special characters
toSpace<-content_transformer(function(x,pattern)gsub(pattern, "", x))
all<-tm_map(all, toSpace, "\\???$")
all<-tm_map(all, toSpace, "\\â$")
all<-tm_map(all, toSpace, "amp")
# Get word frequencies
dtm<-DocumentTermMatrix(all)
dtm

# calculate sum for how many times each term was used
freq<-colSums(as.matrix(dtm))
freq

# find number of distinct terms
length(freq)

#how many times each term appears in each document
m<-as.matrix(dtm)
m

#***TERM SENTIMENT ANALYSIS***

# create document term matrix 
dtm_up = DocumentTermMatrix(VCorpus(VectorSource(all)))
# calcualte frequencies
freq_up <- colSums(as.matrix(dtm_up))
# calculate the sentiment of the terms
sentiments_up = calculate_sentiment(names(freq_up))
# combine the sentiment predictions with the frequencies
sentiments_up = cbind(sentiments_up, as.data.frame(freq_up))
# subset to only include the positive terms in a new variable 
sent_pos_up = sentiments_up[sentiments_up$sentiment == 'Positive',]
# subset to only include the negative terms in a new variable
sent_neg_up = sentiments_up[sentiments_up$sentiment == 'Negative',]

# compare number of positive terms to negative terms
cat("negative terms: ",sum(sent_neg_up$freq_up)," positive terms: ",sum(sent_pos_up$freq_up))

# create word cloud of only the positive words
DT::datatable(sent_pos_up)
positive<-sent_pos_up[,-2]
layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
par(mar=rep(0, 4))
plot.new()
wordcloud(positive$text, positive$freq_up, colors = c( "royalblue","royalblue3", "navyblue"), min.freq=1, max.words = 200)

# create word cloud of only the negative words
DT::datatable(sent_neg_up)
negative <-sent_neg_up[,-2]
layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
par(mar=rep(0, 4))
plot.new()
set.seed(100)

wordcloud(negative$text, negative$freq_up, colors = c("red", "darkred", "firebrick2"), min.freq=1,max.words = 200)
# get dimensions of number of documents and the number of distinct terms
dim(m)

# find frequent terms
findFreqTerms(dtm, lowfreq = 2)

# remove sparse terms that appear in less than 75% of documents
dtms<-removeSparseTerms(dtm, 0.75)
dtms

#***CORRELATION***

# find associations between words found in the same document
findAssocs(dtm, c("great", "ban", "fake"), corlimit=0.20)


#correlation plot
# build with 15 randomly selected words that appear at least 5 times with a correlation of 0.1 or greater
correlation_plot<-plot(dtm, terms=findFreqTerms(dtm, lowfreq = 5)[1:25], corThreshold = 0.1 ,weighting=T)

#***FREQUENCY***

# word frequency plot that appears at least 2 times
wf<-data.frame(word=names(freq), freq=freq)
p<-ggplot(subset(wf, freq>20), aes(word, freq))
p<-p+ geom_bar(stat="identity", colour="black", fill="red3")
p<-p+ theme(axis.text.x = element_text(angle=45, hjust = 1))
all_freq_plot<-p
all_freq_plot

# get frequencies
freq<-colSums(as.matrix(dtm))

# create word cloud
wordcloud(names(freq), freq, min.freq =3000, max.words=350, color=c("royalblue3","red", "red3", "firebrick1", "firebrick2"))


#***CLUSTERING***

# remove the terms that appear less than 95% of documents
dtms<-removeSparseTerms(dtm, 0.95)
# build dissimiliatry matrix and store in d
d<-dist(t(dtms), method = "euclidian")
# use kmeans to build 4 cluster
kfit<-kmeans(d, 4)
# view clusters
kfit
# plot the clusters
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)
# Find cluster details
kfit$cluster

# plot sum of squares distances between cluster and sum of squared distance between clusters for k between 2 and 15
# find optimal level of clusters for kmeans
bss<-integer(length(2:15))
for (i in 2:15) bss[i] <- kmeans(d,centers=i)$betweenss
plot(1:15, bss, type="b", xlab="Number of Clusters",  ylab="Sum of squares", col="blue") 
wss<-integer(length(2:15))
for (i in 2:15) wss[i] <- kmeans(d,centers=i)$tot.withinss
lines(1:15, wss, type="b" )

# build dissimiilarity matrix 
d<-dist(t(dtms), method = "euclidian")
# build dendogram and store in variable fit
fit<-hclust(d=d, method="ward.D2")
# plot dendogram
plot(fit, hang=1)
# add red line to show the cluster each belows to 
plot.new()
plot(fit, hang = -1)
groups<-cutree(fit, k=4)
rect.hclust(fit, k=4, border = "red")

#***LDA***

# make sure each row has at least one term
rowsum.dtm<-apply(dtm, 1, sum)
dtms[rowsum.dtm>0,]
# get the topics using LDA funciton with 6 topics 
lda.trump.notes<-LDA(dtm[rowsum.dtm>0,], k=6, method="Gibbs", control=list(nstart=5, seed=list(101, 102, 103, 104, 105), best=TRUE, burnin=4000, iter=2000, thin=500))
summary(lda.trump.notes)
# get the topic terms
topics.terms<-terms(lda.trump.notes,6)
# create a table of topic terms
knitr::kable(topics.terms)
# create list seperated by commas of terms by topic
topics.terms.lst<-apply(topics.terms, MARGIN = 2, paste, collapse=", ")
write(topics.terms.lst,"", 40, sep = "\n")
# get only the topic number
topics.trump.notes<-topics(lda.trump.notes, 1)
# get how often a topic term is used
topics.term.freq<-rowSums(as.matrix(dtm[rowsum.dtm>0,]))
# combine the topic number and how often the topic term is used into a data frame
topic.df<-data.frame(topics.term.freq, topics.trump.notes)

# create bar plot showing how often each topic appears in the tweets
barplot(table(topic.df$topics.trump.notes), names.arg = c("Topic-1", "Topic-2", "Topic-3", "Topic-4", "Topic-5", "Topic-6"), 
        col = blues9, main = "Trump Tweets Topic By Document", ylab = "Documents")

# density plot of number of terms by number of documents with transparency for better readability
ob<-qplot(topics.term.freq,..count.., data = topic.df, geom = "density", fill=topics.terms.lst[topics.trump.notes], 
          xlab = "Total Terms in Topics", ylab = "Documents Covered By Topics") 
ob<- ob + scale_fill_brewer(palette="RdBu")
ob<-ob+ labs(fill="Topics")
ob<-ob + geom_density(alpha=.3)
ob
