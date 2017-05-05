

# Loading Libaries 


library('zoo')
library('xts')
library('quantmod')
library('TTR')
library('dplyr')
library('xml2')
library('rvest')
library('NLP')
library('RColorBrewer')
library('wordcloud')

library('tm')
library('SnowballC')
library('RWeka')
library('rJava')
library('RWekajars')
library('sentimentr')


# Downloading News from Kaggle Database
library(readr)
Combined_News_DJIA <- read_csv("~/Google Drive/MASTER/CUARTO SEMESTRE/TRABAJO FIN MASTER/Trabajo Fin master Docs/Combined_News_DJIA.csv")
View(Combined_News_DJIA)

news <- Combined_News_DJIA



# Scrapping time series of Stock Index for the S&P500
# Getting the Stock Market data Index for the S&P 500. Source: Yahoo

getSymbols("^GSPC",src="yahoo") #loading the S&P500 "environment"

index_sp <- GSPC$GSPC.Adjusted
plot(index_sp)
# Creating a dataframe with index and date variables
index <-  data.frame(date=index(index_sp), coredata(index_sp))


# selecting observations for analysis - just Dates contained in both dataframes
index <- index[c(which(index$date == min(news$Date)):which(index$date == max(news$Date))),]


#---------------------------------------#
#                                       #
#       Text Mining Analysis            #
#                                       #
#---------------------------------------#
# http://www.rdatamining.com/examples/text-mining
# Merging all headlines into one big variable
vars <- c(names(news[,c(3:27)]))
news$all_news <- apply( news[ , vars ] , 1 , paste , collapse = "-" )
colnames(news)[28] <- "all_news"

# Creating the corpus to the dataframe
myCorpus <- Corpus(VectorSource(news$all_news))

myCorpus <- tm_map(myCorpus, tolower)
# remove punctuation
myCorpus <- tm_map(myCorpus, removePunctuation)
# remove numbers
myCorpus <- tm_map(myCorpus, removeNumbers)
# remove stopwords
# keep "r" by removing it from stopwords
myStopwords <- c(stopwords('english'), 
                 "available", "via", "b",
                 "%","^","|","and","for")
idx <- which(myStopwords == "r")
myStopwords <- myStopwords[-idx]


#=======================================#
#       STEMMING WORDS                  #
#=======================================#

dictCorpus <- myCorpus
# stem words in a text document with the snowball stemmers,
# which requires packages Snowball, RWeka, rJava, RWekajars
myCorpus <- tm_map(myCorpus, stemDocument)
# inspect the first three ``documents"
inspect(myCorpus[1:3])

# stem completion
myCorpus <- tm_map(myCorpus, stemCompletion, dictionary=dictCorpus)

#=======================================#
#       BUILDING DTM                    #
#=======================================#

myDtm <- TermDocumentMatrix(myCorpus, control = list(stopwords=TRUE,minWordLength = 1))
inspect(myDtm[266:270,31:40])

#=======================================#
#       Wordcloud                       #
#=======================================#
m <- as.matrix(myDtm)
# calculate the frequency of words
v <- sort(rowSums(m), decreasing=TRUE)
myNames <- names(v)
k <- which(names(v)=="miners")
myNames[k] <- "mining"
d <- data.frame(word=myNames, freq=v)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


# https://cran.r-project.org/web/packages/sentimentr/sentimentr.pdf

sentiment(news[1,28] , polarity_dt = lexicon::hash_sentiment_jockers,
          valence_shifters_dt = lexicon::hash_valence_shifters, 
          hyphen = "",
          amplifier.weight = 0.8, n.before = 5,
          n.after = 2,
          question.weight = 1,
          adversative.weight = 0.85,
          missing_value = 0
          )








