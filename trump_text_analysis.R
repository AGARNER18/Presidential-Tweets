# load packages

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



# load trump tweets during his presidency so far

read.csv("trump_tweets.csv")



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



# emotion analysis on some_txt

emotion<-classify_emotion(some_txt, algorithm = "bayes", prior=1.0, verbose = TRUE)



# get only the emotion predictions

emotion_2 = emotion[,7]



# change NA emotions to unknown

emotion_2[is.na(emotion_2)] = "unknown"



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



# create document term matrix

dtm_up = DocumentTermMatrix(VCorpus(VectorSource(all)))

freq_up <- colSums(as.matrix(dtm_up))

# get sentiments

sentiments_up = calculate_sentiment(names(freq_up))

# add sentiments to data frame

sentiments_up = cbind(sentiments_up, as.data.frame(freq_up))

# creat new data frame with only the positive words

sent_pos_up = sentiments_up[sentiments_up$sentiment == 'Positive',]

# create new data frame with only the negative words

sent_neg_up = sentiments_up[sentiments_up$sentiment == 'Negative',]

# compare count of positive versus negative words

cat("We have far lower negative Sentiments: ",sum(sent_neg_up$freq_up)," than positive: ",sum(sent_pos_up$freq_up))

# keep only the positive terms and not the column of sentiments

DT::datatable(sent_pos_up)

positive<-sent_pos_up[,-2]

# keep only positive terms with a frequency of at least 2

positive<-positive[positive$freq_up>2,]



# create word cloud of positive words

layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))

par(mar=rep(0, 4))

plot.new()

set.seed(100)

patriotic<-c("red","blue", "red3", "red4", "royalblue", "royalblue4")

wordcloud2(positive, size=2.3,minRotation = -pi/6, maxRotation = -pi/6, rotateRatio = 1, color=rep_len(c("red", "blue", "red3", "royalblue"), nrow(positive)))



# create word cloud of the negative words

DT::datatable(sent_neg_up)

negative <-sent_neg_up[,-2]

layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))

par(mar=rep(0, 4))

plot.new()

set.seed(100)

wordcloud2(negative,size=2.3,minRotation = -pi/-6, maxRotation = -pi/-6, rotateRatio = 1,color=rep_len( c("red","blue", "red3", "red4", "royalblue", "royalblue4"),nrow(negative)))



# get dimensions of number of documents and the number of distinct terms

dim(m)



# find frequent terms

findFreqTerms(dtm, lowfreq = 2)



# remove sparse terms that appear in less than 5% of documents

dtms<-removeSparseTerms(dtm, 0.75)

dtms



# find associations between words found in the same document

findAssocs(dtm, c("gun", "ban", "putin"), corlimit=0.30)





#correlation plot



# build with 25 randomly selected words that appear at least 3 times with a correlation of 0.1 or greater

correlation_plot<-plot(dtm, terms=findFreqTerms(dtm, lowfreq = 3)[1:25], corThreshold = 0.1, weighting=T)



# word frequency plot that appears at least 2 times

wf<-data.frame(word=names(freq), freq=freq)

p<-ggplot(subset(wf, freq>15), aes(word, freq))

p<-p+ geom_bar(stat="identity", colour="black", fill="red3")

p<-p+ theme(axis.text.x = element_text(angle=45, hjust = 1))

all_freq_plot<-p

all_freq_plot



# create word cloud

wordcloud(names(freq), freq, min.freq = 200)



freq<-colSums(as.matrix(dtm))

# create color set to be used in the word cloud

# add colors to the word cloud

wordcloud(names(freq), freq, min.freq = 250, colors = dark2)

wordcloud(names(freq), freq, min.freq =3000, max.words=350, color=c("royalblue3","red", "red3", "firebrick1", "firebrick2"))

# k-means clustering

# cluster based on their appearance together

# remove the terms that appear less than 90% of documents

dtms<-removeSparseTerms(dtm, 0.95)

# build dissimiliatry matrix and store in d

d<-dist(t(dtms), method = "euclidian")

# use kmeans to build 3 cluster

kfit<-kmeans(d, 5)

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

groups<-cutree(fit, k=5)

rect.hclust(fit, k=5, border = "red")