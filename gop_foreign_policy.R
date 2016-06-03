library(tm)
library(ggplot2)
library(stringr)

library(topicmodels)

gop <- read.csv('Sentiment.csv')
gop$subject_matter <- as.character(gop$subject_matter)
gop_for <- subset(gop,subject_matter=='Foreign Policy')
date<- strptime(gop_for$tweet_created,"%m/%d/%Y")
tweet <- as.character(gop_for$text)
#cleaning the tweets
tweet = gsub("(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", " ", tweet)
#retweet
tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", tweet)
# removing hashtags
tweet = gsub("#\\w+", " ", tweet)
# removing @people
tweet = gsub("@\\w+", " ", tweet)
#removing punctuations
tweet = gsub("[[:punct:]]", " ", tweet)
#removing numbers
tweet = gsub("[[:digit:]]", " ", tweet)
#removing emojis
tweet<-str_replace_all(tweet,"[^[:graph:]]"," ")

#removing spaces
tweet = gsub("[ \t]{2,}", " ", tweet)
tweet = gsub("^\\s+|\\s+$", "", tweet)
tweet = tolower(tweet)
# Converting to corpus and removing stopwords
corp <- Corpus(VectorSource(tweet))
# char <- as.character(gop_for$candidate)
# char <- char[char!='No candidate mentioned']
# char <- tolower(char)
corp <- tm_map(corp,removeWords,c(stopwords('english'),stopwords('SMART'),'cruz','trump','walker'))
# term document Matrix
tdm <- TermDocumentMatrix(corp)
frequency <- rowSums(as.matrix(tdm))
frequency<- subset(frequency,frequency>=10)
df <- data.frame(terms=names(frequency),freq = frequency)
ggplot(df, aes(x = terms, y = freq)) + geom_bar(stat = "identity") + xlab("Terms") + ylab("Count") + coord_flip()
#Associations
findAssocs(tdm,'russia',0.2)
findAssocs(tdm,'china',0.2)
findAssocs(tdm,'israel',0.2)
findAssocs(tdm,'iran',0.2)
#Topic Modelling
dtm <- as.DocumentTermMatrix(tdm)
lda <-LDA(dtm,k=5)
term<-terms(lda,6)
term <- apply(term, MARGIN = 2, paste, collapse = ", ")
topic <- topics(lda,1)
topics <- data.frame(time=as.POSIXct.default(gop_for$tweet_created), topic)
qplot(time, ..count.., data=topics, geom="density",
      fill=term[topic], position="stack")
################################################################################
#location
loc <- as.character(levels(gop$tweet_coord))
loc <- str_replace_all(loc ,'\\[|]','')
loc <- loc[loc!='']
loc <- strsplit(loc,',')
loc <- unlist(loc)

odd <- seq(1,length(loc),2)
even <- seq(2,length(loc),2)
lat<- as.numeric(loc[odd])
long <- as.numeric(loc[even])
df_loc<-data.frame(latitude=lat,longitude=long)
library(rworldmap)
newmap <- getMap(resolution='high')
plot(newmap)
points(df_loc$longitude,df_loc$latitude,col='red',cex=1.0)
