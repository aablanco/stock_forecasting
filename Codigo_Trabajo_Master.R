

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
library('SentimentAnalysis')
library('ggplot2')
library("corrplot")

library('tm')
library('SnowballC')
library('rJava')
library('RWekajars')
library('sentimentr')
library('readr')

# Downloading News from Kaggle Database

Combined_News_DJIA <- read_csv("~/Google Drive/MASTER/CUARTO SEMESTRE/TRABAJO FIN MASTER/Trabajo Fin master Docs/Combined_News_DJIA.csv")



# Scrapping time series of Stock Index for the S&P500
# Getting the Stock Market data Index for the S&P 500. Source: Yahoo

getSymbols("^GSPC",src="yahoo") #loading the S&P500 "environment"

index_sp <- GSPC$GSPC.Adjusted
plot(index_sp)
# Creating a dataframe with index and date variables
index <-  data.frame(date=index(index_sp), coredata(index_sp))


# selecting observations for analysis - just Dates contained in both dataframes
index <- index[c(which(index$date == min(news$Date)):which(index$date == max(news$Date))),]

#=======================================#
#       PLOT INDEX                      #
#=======================================#


plot(index,type = "l", col = "blue", xlab = "Date", ylab = "Index",
     main = "S&P500 Index")



#---------------------------------------#
#                                       #
#       Text Mining Analysis            #
#                                       #
#---------------------------------------#
# http://www.rdatamining.com/examples/text-mining
# Merging all headlines into one big variable for posterior analysis

news <- Combined_News_DJIA
vars <- c(names(news[,c(3:27)]))
news$all_news <- apply( news[ , vars ] , 1 , paste , collapse = "-" )
colnames(news)[28] <- "all_news"

# Creating the corpus to the dataframe
myCorpus <- Corpus(VectorSource(news$all_news))

# creating a function to erase special characters
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
# put space instead of special characters
myCorpus <- tm_map(myCorpus, toSpace, "/ | @ | \b |-")

myCorpus <- tm_map(myCorpus, tolower)
# remove punctuation
myCorpus <- tm_map(myCorpus, removePunctuation)
# remove numbers
myCorpus <- tm_map(myCorpus, removeNumbers)
# remove stopwords
myStopwords <- c(stopwords('english'))

# Strip Whitespace
myCorpus <- tm_map(myCorpus, stripWhitespace)


#=======================================#
#       BUILDING DTM                    #
#=======================================#

myDtm <- TermDocumentMatrix(myCorpus, control = list(stopwords=TRUE,minWordLength = 3))
inspect(myDtm[266:270,31:40])

# calculate the dimensions of TDM
dim(myDtm)


# TDM frequency count
matrix_myDtm<-as.matrix(myDtm)
freq <- sort(rowSums(matrix_myDtm), decreasing=TRUE)
word_freq <- data.frame(word=names(freq), freq=freq)

#=======================================#
#       Frequency Plot                  #
#=======================================#

# create new dataframe of tdm
freq_words <- data.frame(word=word_freq[,1], freq= word_freq[,2])
# now sorting it and cutting it 
freq_words <- subset(freq_words[with(freq_words, order(-freq)),],freq>300) 

#plotting frequency of words
p <- ggplot(subset(freq_words, freq>1000), aes(word, freq))    
p <- p + geom_bar(stat="identity")   
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
p 



#=======================================#
#       Wordcloud                       #
#=======================================#

set.seed(1234)
wordcloud(words = freq_words$word, freq = freq_words$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))




# creating a dataframe with the sentiment of the news
temp = as.data.frame(sentiment(news$all_news))
# aggregate the mean sentiment polarity for each element
sentiment_master = data.frame(news$Date, aggregate(temp$sentiment, list(temp$element_id), mean))
colnames(sentiment_master)[1] <- "Date"
colnames(sentiment_master)[3] <- "polarity_score"
sentiment_master <- sentiment_master[,-c(2)]

# converting master sentiment into data frame
#sentiment_master <- as.data.frame(sentiment_master)
# merging it with the data
news <- merge(news,sentiment_master,by.x = "Date",by.y = "Date")

# plot histogram for polarity score
hist(news$polarity_score, 
     main="Histogram for Polarity Score", 
     xlab="polarity score", 
     ylab = "polarity score",
     border="black", 
     col="grey",
     xlim=c(-2.0,0.4)
     )


#=======================================#
#       Processing Index                #
#=======================================#

# changing name of S&P index
colnames(index)[2] <- "sp_index"

# merging index dataset and news dataframe
df_complete <- merge(news,index,by.x = "Date",by.y = "date")

# decompose elements of index
dec = ts(index$sp_index,frequency = 365)
fit <- decompose(dec)
#fit = stl(dec, s.window="periodic")
plot(fit)

fit <- data.frame(sp_index = fit$x,index_seasonal = fit$seasonal,index_trend = fit$trend,index_random = fit$random)

df_complete <- merge(df_complete,fit,by.x = "sp_index",by.y = "sp_index")



# Show autocorrelation function 
acf(df_complete$sp_index)

# take diferences of index 
index_dif = index$sp_index
index_dif <- data.frame(diff(log(index_dif), differences=1),index$date[2:1989])
colnames(index_dif)[1] <- "variation"
colnames(index_dif)[2] <- "Date"
# show autocorrelation index
acf(index_dif$variation)

# plot differences
plot(index_dif$Date,index_dif$variation,type = "o", col = "blue", xlab = "Date", ylab = "Variation",
     main = "Variation S&P500 Index")
# merging again all data
df_complete <- merge(df_complete,index_dif,by.x = "Date",by.y = "Date")

# taking portion of data 
df_portion = data.frame(Date = df_complete$Date,
                        sentiment = df_complete$sentiment,
                        sp_index = df_complete$sp_index,
                        variation = df_complete$variation,
                        index_seasonal = df_complete$index_seasonal,
                        index_trend = df_complete$index_trend,
                        index_random = df_complete$index_random
                        )


# Ploting Variation against time
plot(df_portion$Date, df_portion$variation, type="o", col="blue", ylab="Variation",xlab="Time",main="Variation Plot")
# Ploting sentiment against time
plot(df_portion$Date, df_portion$sentiment, type="o", col="blue", ylab="Sentiment",xlab="Time",main="Sentiment Plot")

M <- cor(df_portion[,3:5])
corrplot(M, method="circle")



# Function to lag a variable within a dataframe
lagpad <- function(x, k) {
  if (!is.vector(x)) 
    stop('x must be a vector')
  if (!is.numeric(x)) 
    stop('x must be numeric')
  if (!is.numeric(k))
    stop('k must be numeric')
  if (1 != length(k))
    stop('k must be a single number')
  c(rep(NA, k), x)[1 : length(x)] 
}

# Adding lags to the function
df_portion <- data.frame(df_portion
                              ,lag_var1 = lagpad(df_portion$variation,1)
                              ,lag_var2 = lagpad(df_portion$variation,2)
                              ,lag_var3 = lagpad(df_portion$variation,3)
                              ,lag_var4 = lagpad(df_portion$variation,4)
                              ,lag_var5 = lagpad(df_portion$variation,5)
                              ,lag_var6 = lagpad(df_portion$variation,6)
                              ,lag_var7 = lagpad(df_portion$variation,7)
                              ,lag_var8 = lagpad(df_portion$variation,8)
                              ,lag_var9 = lagpad(df_portion$variation,9)
                              ,lag_var10 = lagpad(df_portion$variation,10)
                   )

# Just taking the complete cases 
df_portion = df_portion[complete.cases(df_portion),]
# Again computing correlations
M <- cor(df_portion[,2:17])
corrplot(M, method="circle")

# Checking for correlation with the variation of the trend component
temp = df_portion
temp <- data.frame(trend_dif = diff(log(df_portion$index_trend), differences=1),
                   Date = df_portion$Date[1:1643],
                   sentiment = df_portion$sentiment[1:1643])

M <-cor(data.frame(trend = temp$trend_dif, sentiment = temp$sentiment))
corrplot(M, method="circle")

# Checking for some co-movement
df_portion$dummy_variation = ifelse(df_portion$variation > 0, 1, 0)
df_portion$dummy_sentiment = ifelse(df_portion$sentiment > 0, 1, 0)

M <- cor(df_portion[,18:19])
corrplot(M, method="circle")

# CCF for sentiment and variation of index
ccf(df_portion$sentiment,df_portion$variation,main="Sentiment and Index variation")
