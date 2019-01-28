#Charlottesville on Twitter/Unite the Right Rally 

#load library
library(ggplot2)
library(readr)
library(dplyr)
library(tm)
library(tidytext)
library(tidyverse)
library(wordcloud)
library(wordcloud2)
library(tidyr)
install.packages("chron") # for timestamp mngt
library(chron)
library(syuzhet)

#load data through import dataset
#aug15_sample, aug16_sample, aug17_sample, and aug18_sample - 50000 obs each and 24 variables
aug15_sample <- read_csv("~/Downloads/charlottesville-on-twitter/aug15_sample.csv")
aug16_sample <- read_csv("~/Downloads/charlottesville-on-twitter/aug16_sample.csv")
aug17_sample <- read_csv("~/Downloads/charlottesville-on-twitter/aug17_sample.csv")
aug18_sample <- read_csv("~/Downloads/charlottesville-on-twitter/aug18_sample.csv")

#view data
head(aug15_sample)
str(aug15_sample)

#bind all data together
alltweets <- rbind(aug15_sample, aug16_sample, aug17_sample, aug18_sample)

#save and export the alltweets as csv file
write.csv(alltweets, file = "charlottealltweets.csv")

#check data
alltweets %>% select('screen_name', 'full_text', 'created_at') %>%
  sample_n(size = 20)

# 1. Descriptive stats 
# group by user id - who tweeted the most? some people tweeted more than once on the issue
d <- alltweets %>% 
  group_by(user_id) %>%
  summarize(total = n())

d

plot(d$total, xlab = "User ID", ylab = "Number of Tweets", main = "Number of Tweets by Users: Aug 15 - 18 Charlottesville Rally")

# How many tweets?
length(levels(alltweets$user_name))

# Top 20 characters with more dialogues 
top.alltweets.chars <- as.data.frame(sort(table(alltweets$user_name), decreasing=TRUE))[1:20,]

# Lets plot them! 
ggplot(data=top.alltweets.chars, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity", fill="#2e557c", color="white") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=45, hjust=1, size = 12)) +
  labs(x="User Name", y="Number of tweets") +
  ggtitle("Number of Tweets by Users: August 2017 Charlottesville Rally")


# user location - where were the most tweets coming from? Answer: unknown, United States, USA, Washington, DC, Los Angeles, CA
l <- alltweets %>%
  group_by(user_location) %>%
  summarize(total = n()) %>%
  arrange(desc(total)) %>%
  top_n(20)

l

# Q. Need to combine the observations of user location with different names i.e United States with USA, New York, NY = New York, USa and New York, US = New York
levels(alltweets$user_location)

# Copy the region column
newcol <- alltweets$user_location

# Define the words you want to remove
newcol <- removeWords(newcol,c("United States = USA", "New York, USA = New York, NY"))

# Replace the column back in
alltweets$user_location <-newcol

# Check the file
head(alltweets$user_location) 

#plot again to check if observations of user location merged correctly - nope didnt merge
l2 <- alltweets %>%
  group_by(user_location) %>%
  summarize(total = n()) %>%
  arrange(desc(total)) %>%
  top_n(20)

l2

#plot - basically most people tweeted from the U.S, a lot of unknown user location, some from earth :)
ggplot(l2, aes(reorder(user_location, total), total, fill = total)) +
  geom_col() +
  ggtitle("User Location of Charlottesville Rally Tweets: Aug 15 - 18, 2017") +
  coord_flip()


# followers count

f <- alltweets %>%
  group_by(followers_count) %>%
  summarize(total = n()) %>%
  arrange(desc(total)) %>%
  top_n(20)

f

#plot 
ggplot(f, aes(reorder(followers_count, total), total, fill = total)) +
  geom_col() +
  ggtitle("User Followers Count, Charlottesville Rally Tweets: Aug 15 - 18, 2017") +
  coord_flip()

# weird numbers here, doesn't make sense


# 2. Sentiment Analysis

#sentiment w/o cleaning the text - for aug 15 tweets
aug15_sample$full_text= as.character(aug15_sample$full_text)

#Get nrc sentiments
s <-get_nrc_sentiment(aug15_sample$full_text)
td <-data.frame(t(s))
td_new <- data.frame(rowSums(td[1:50000]))
names(td_new)[1] <- "count"
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL

#plot
qplot(sentiment, data=td_new, weight=count, geom="bar",fill=sentiment)+
  ggtitle("Aug 15th Charlottesville Rally Sentiments")

#sentiment analysis of all tweets - no difference from aug 15 tweets
alltweets$full_text= as.character(alltweets$full_text)

#Get nrc sentiments
al <-get_nrc_sentiment(alltweets$full_text)
td2 <-data.frame(t(al))
td_new2 <- data.frame(rowSums(td2[1:50000]))
names(td_new2)[1] <- "count"
td_new2 <- cbind("sentiment" = rownames(td_new2), td_new2)
rownames(td_new2) <- NULL

#plot
qplot(sentiment, data=td_new2, weight=count, geom="bar",fill=sentiment)+
  ggtitle("August 2017 Charlottesville Rally Sentiments")

#could analyze sentiments using Bing or Afinn lexicons
#polarity tests,
#subset of sentiments

library(dplyr)
#get nrc sentiments
get_sentiments("nrc")

#count the no. of sentiments 
get_sentiments("nrc") %>%
  count(sentiment)

nrc <- get_sentiments("nrc")
#inner join
td_new2 %>%
  # With inner join, implement sentiment analysis using nrc
  inner_join(nrc)

#what are the most common negative words? #error
negative_words <- td_new2 %>%
  # Filter to choose only words associated with joy
  filter(sentiment == "negative") %>%
  # Group by each word
 
  # Use the summarize verb to find the mean frequency
  summarize(freq = mean(freq)) %>%
  # Arrange to sort in order of descending frequency
  arrange(desc(freq)) 


# Word frequency and word clouds
#clean the data - aug 15 tweets
# Construct a corpus, and specify the source to be characters by row (aka character vectors)
corpus <- Corpus(VectorSource(aug15_sample$full_text))
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

# Remove Extra Text (how to remove u2753?, row 29) also had to remove charlottesville
cleanset <- tm_map(cleanset, removeWords, c("&amp", "amp", "what", "isnt", "used", "hes", "n", "put", "can", "cant",
                                            "use", "like","one","must", "will", "doesnt", "tfw", "u2753", "motherfucking",
                                            "u", "raci\nleft", "dam", "wh", "fuck", "via", "now", "tweet", "sides", "charlottesville",
                                            "youre", "ever", "really", "please"))

inspect(cleanset[1:40])

# Replace words (advice: go plural -> singular): 
cleanset <- tm_map(cleanset, gsub, pattern = "trumps", replacement = "Trump")
cleanset <- tm_map(cleanset, gsub, pattern = "medias", replacement = "media")
cleanset <- tm_map(cleanset, gsub, pattern = "identifies", replacement = "identify")
cleanset <- tm_map(cleanset, gsub, pattern = "trends", replacement = "trend")
cleanset <- tm_map(cleanset, gsub, pattern = "terrorists", replacement = "terrorist")
cleanset <- tm_map(cleanset, gsub, pattern = "remarks", replacement = "remark")
cleanset <- tm_map(cleanset, gsub, pattern = "protests", replacement = "protest")
cleanset <- tm_map(cleanset, gsub, pattern = "nationals", replacement = "national")
cleanset <- tm_map(cleanset, gsub, pattern = "muslims", replacement = "muslim")

# Remove Extra White Space
cleanset <- tm_map(cleanset, stripWhitespace)
inspect(cleanset[1:20])

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

# Number Of Times A Term Appears - #charlottesville 26102, trump 11458, violence 2577 (top 95%)
termFreqency <- rowSums(tdm)
termFreqency

# Subset frequent words 
# had to edit and remove some words such as charlottesville, now, via, tweet, just (though this tricky) that must have not been removed while cleaning ealier.
termFreqency <- subset(termFreqency, termFreqency>1000)
termFreqency

# Bar Plot
barplot(termFreqency, las=2, col = rainbow(30), main = "August 15th Charlottesville Tweets: Frequent Words")


# If the plot labels need to be shifted
sas <- barplot(termFreqency, axes = TRUE, axisnames = FALSE, las=2, col=rainbow(30), 
                    main = "August 15th Charlottesville Tweets: Frequent Words")
text(sas[,1], -1.0, srt = 60, adj = c(1.1,1.1), xpd = TRUE, labels = names(termFreqency) , cex=0.6)

# Wordcloud 
# max.words will plot the specified number of words and discard least frequent terms
# min.freq will discard all terms whose frequency is below the specified value

install.packages("wordcloud")
library(wordcloud)

wordFreq <- sort(rowSums(tdm), decreasing = TRUE)
set.seed(1234)
wordcloud(words = names(wordFreq), freq = wordFreq, random.order = F, max.words = 100, 
          colors = brewer.pal(6, 'Dark2'), rot.per = 0.35, main = "August 15th Charlottesville Tweets: Word Cloud")

# Rotate Some Words - not necessary
wordcloud(words = names(wordFreq), freq = wordFreq, random.order = F, min.freq = 50, 
          colors = brewer.pal(6, 'Dark2'), scale = c(3.2,0.2), rot.per = 0.8)

#comparison cloud - didnt work, crashed R
comparison.cloud(tdm, max.words = 50, colors =  c("blue", "red"))

# More options with (wordcloud2)
install.packages("wordcloud2")
library(wordcloud2)
wordFreq <- data.frame(names(wordFreq),wordFreq)
colnames(wordFreq) <- c("word","freqency")
wordcloud2(wordFreq, size = 0.5, shape = "star", rotateRatio = 0.5)

# Plot the set using ggplot2
library(ggplot2)

#change to data frame
df <- data.frame(term = names(termFreqency), freq = termFreqency)

#plot a bargraph

ggplot(df, aes(x = term, y = freq)) +
  geom_bar(stat = "identity") +
  ggtitle("August 15th Charlottesville Tweets: Frequent Words") +
  xlab("Terms") +
  ylab("Count") +
  coord_flip()
