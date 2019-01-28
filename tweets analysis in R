# Twitter Analysis - EXTRACTING AND CLEANING TEXT

# Load the text mining package
library(tm)

# Getting Tweets - Direct option 
# Step 1: Load these libraries
library(twitteR)
library(ROAuth)

# Step 2: Twitter authentication - Must be completed at https://apps.twitter.com/
consumer_key = "Bn165urHwEJ1Kxt6nlD6xPTCR" 
consumer_secret = 	"CeMDuxdKikMePES6jwzGvhTYTj81E1XAgaueQ4zACUAvyfdDkf"
access_token = "89509276-ZjheZxWOet9w4EODwyhEv3hOTMLiSX82LpvYKG0CN"
access_secret = 	"wiJSA015krqu0DKPDjUjLDI0V1fysbc6AadnjVa6NvScr"

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

# Step 3: Access Tweets (3200 limit per session)

tweets <- userTimeline("ExtremePropa", n = 3200)

library(plyr)

ExtremePropa = laply(tweets, function(t) t$getText())
as.data.frame(ExtremePropa)
write.csv(ExtremePropa, file="ExtremePropa.csv", row.names = FALSE, col.names = FALSE)

#Analyze the extracted tweets using the csv file of them
mydata <- read.csv(file.choose(""), 
                   header = T,stringsAsFactors=FALSE)
tweets=lapply(mydata$Text, function(x) iconv(x, "latin1", "ASCII", sub=""))
head(mydata, n = 10)

# Construct a corpus, and specify the source to be characters by row (aka character vectors)
corpus <- Corpus(VectorSource(mydata))
inspect(corpus[1:5])

# Clean Text
cleanset <- tm_map(corpus, tolower) # Convert all text to lower case
inspect(cleanset[1:10])

cleanset <- tm_map(cleanset, removePunctuation) # Remove all puncutation
inspect(cleanset[1:10])

cleanset <- tm_map(cleanset, removeNumbers) # Remove all numbers
inspect(cleanset[1:10])

removeURL <- function(x) gsub("http[^[:space:]]*", "", x) # Remove URL function
cleanset <- tm_map(cleanset, content_transformer(removeURL)) # Remove URLs (both http and https)
inspect(cleanset[1:10])

cleanset <- tm_map(cleanset, removeWords, stopwords("english")) # Remove stopwords
inspect(cleanset[1:10])

# Remove Extra Text
cleanset <- tm_map(cleanset, removeWords, c("&amp", "cwhat", "vp", "used", "wcco", "put", "can", "cant",
                                            "use", "like","one","must", "will", "doesnt"))

inspect(cleanset[1:10])

# Replace words (advice: go plural -> singular): 
cleanset <- tm_map(cleanset, gsub, pattern = "brothers", replacement = "brother")
cleanset <- tm_map(cleanset, gsub, pattern = "suggests", replacement = "suggest")
cleanset <- tm_map(cleanset, gsub, pattern = "identifies", replacement = "identify")
cleanset <- tm_map(cleanset, gsub, pattern = "trends", replacement = "trend")
cleanset <- tm_map(cleanset, gsub, pattern = "terrorists", replacement = "terrorist")
cleanset <- tm_map(cleanset, gsub, pattern = "others", replacement = "other")
cleanset <- tm_map(cleanset, gsub, pattern = "protests", replacement = "protest")
cleanset <- tm_map(cleanset, gsub, pattern = "nationals", replacement = "national")
cleanset <- tm_map(cleanset, gsub, pattern = "muslims", replacement = "muslim")

# Remove Extra White Space
cleanset <- tm_map(cleanset, stripWhitespace)
inspect(cleanset[1:10])

# Term Document Matrix
# This line of code converts all text in the corpus to PlainTextDocument,
# on which the DocumentTermMatrix function does not work properly: 
# cleanset <- tm_map(cleanset, PlainTextDocument)
tdm <- TermDocumentMatrix(cleanset, control = list(WordLength=c(1,Inf)))

# Review the summary of tdm
tdm
# Option to limit what the tdm calculates. Below we will only account for the upper 95% of data.
tdm <- removeSparseTerms(tdm, 0.95)
# View tdm via a matix
tdm <- as.matrix(tdm)
tdm[1:10, 1:20]  

# Number Of Times A Term Appears
termFreqency <- rowSums(tdm)
termFreqency

# Subset frequent words
termFreqency <- subset(termFreqency, termFreqency>70)
termFreqency

# Bar Plot
barplot(termFreqency, las=2, col=rainbow(30))

# If the plot labels need to be shifted
sas <- barplot(termFreqency, axes = TRUE, axisnames = FALSE, las=2, col=rainbow(30))
text(sas[,1], -1.0, srt = 60, adj = c(1.1,1.1), xpd = TRUE, labels = names(termFreqency) , cex=0.6)

# Wordcloud 
# - max.words will plot the specified number of words and discard least frequent terms
# - min.freq will discard all terms whose frequency is below the specified value

install.packages("wordcloud")
library(wordcloud)

wordFreq <- sort(rowSums(tdm), decreasing = TRUE)
set.seed(123)
wordcloud(words = names(wordFreq), freq = wordFreq, random.order = F, max.words = 50, 
          colors = brewer.pal(6, 'Accent'), scale = c(3.2,0.2), rot.per = 0.0)

# Rotate Some Words
wordcloud(words = names(wordFreq), freq = wordFreq, random.order = F, min.freq = 50, 
          colors = brewer.pal(6, 'Dark2'), scale = c(3.2,0.2), rot.per = 0.8)


# More options with (wordcloud2)
install.packages("wordcloud2")
library(wordcloud2)
wordFreq <- data.frame(names(wordFreq),wordFreq)
colnames(wordFreq) <- c("word","freqency")
wordcloud2(wordFreq, size = 0.5, shape = "star", rotateRatio = 0.5)


# Plot the set using ggplot2
library(ggplot2)
df <- data.frame(term = names(termFreqency), freq = termFreqency)
ggplot(df, aes(x = term, y = freq)) + geom_bar(stat = "identity") +
  xlab("Terms") + ylab("Count") + coord_flip()


# Twitter Analysis - SENTIMENT ANALYSIS AND STRUCTURAL NETWORKS

# Load packages
install.packages("syuzhet")
library(syuzhet)
install.packages("lubridate")
library(lubridate)
install.packages("scales")
library(scales)
library(reshape2)
install.packages("dplyr")
library(dplyr)

# Load a Twitter file
mydata <- read.csv(file.choose(""), 
                   header = T,stringsAsFactors=FALSE)
extreme = lapply(mydata$Text, function(x) iconv(x, "latin1", "ASCII", sub=""))
extreme <- as.character(extreme)

# Get sentiment scores

sent <- get_nrc_sentiment(mydata)
head(sent)
mydata[4]

mydata <- get_nrc_sentiment(extreme)
head(mydata)
extreme[4]

# Check the score for a word
get_nrc_sentiment('news')

# Construct a bar plot
barplot(colSums(mydata), las = 2, col = rainbow(10), ylab = "Count", 
        main = "Sentiment Scores for Extremist Tweets")

tw <- barplot(colSums(mydata), axes = TRUE, axisnames = FALSE, las=2, col = rainbow(10), 
              ylab = "Count", main = "Sentiment Acores for Extremist Tweets")
text(tw[,1], -1.0, srt = 60, adj = c(1.1,1.1), xpd = TRUE, labels = names(sent), cex=0.6)

# how terms are connected
source("http://www.bioconductor.org/biocLite.R") 
#from https://www2.warwick.ac.uk/fac/sci/moac/people/students/peter_cock/r/rgraphviz/

biocLite("graph")
biocLite("Rgraphviz")

library(graph)
library(Rgraphviz)


# Discover where terms interact
idx <- which(dimnames(tdm)$Terms %in% c("terrorist", "islam"))
idx
as.matrix(tdm[idx,1:10])

# Find frequent terms
tdm <- TermDocumentMatrix(cleanset, control = list(WordLength=c(1,Inf)))
(freq.terms <- findFreqTerms(tdm, lowfreq = 70))

# Find network of terms
plot(tdm, term = freq.terms, corThreshold = 0.03, weighting = T)


