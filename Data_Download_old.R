

# Loading Libaries 


library('zoo')
library('xts')
library('quantmod')
library('TTR')
library('dplyr')
library('xml2')
library('rvest')

# Getting the Stock Market data Index for the S&P 500. Source: Yahoo


getSymbols("^GSPC",src="yahoo") #loading the S&P500 "environment"

index_sp <- GSPC$GSPC.Adjusted
plot(index_sp)


# Scraping Yahoo Headlines



web_address<-'https://finance.yahoo.com/quote/%5EGSPC?p=^GSPC'

# Read the html code of the webpage in to the variable webpage_code
webpage_code<-read_html(web_address)

# Read the headlines of the News.com.au portal using the CSS path identified using the Selector Gadget
news_headlines <- html_nodes(webpage_code,"#quoteNewsStream-0-Stream")

# Convert the new headlines to text and view the summary data
news_headlines_text <- html_text(news_headlines)
head(news_headlines_text)

# Create a data frame with the headlines and the description of the news
df <- data.frame(news_headlines_text)
Need_to_know<-data.frame(do.call('rbind', strsplit(as.character(df$news_headlines_text),'•',fixed=TRUE)))

# view the structure of the data frame
str(Need_to_know)








doc <- read_html("https://finance.yahoo.com/quote/%5EGSPC?p=^GSPC")
scope <- doc %>% html_nodes(".StretchedBox")
res <- lapply(scope, function(li){
  data.frame(stringsAsFactors = FALSE,
             date = li %>% html_node("cite span") %>% html_text,
             headline = li %>% html_node("a") %>% html_text
  )
})
do.call(rbind, res)

test <- data.frame(res)

