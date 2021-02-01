rm(list = ls())
#Preliminaries####
setwd("~/Stock Average Price Projection")
stock_symbols <- read_csv("(today)stock_symbols.csv", col_names = FALSE)
stock_symbols <- stock_symbols[,1]

stock_symbols <- gsub('"',"", stock_symbols$X1)
stock_symbols <- as.data.frame(stock_symbols)
stock_symbols <- sub(".*? ", "", stock_symbols$stock_symbols)
stock_symbols <- as.data.frame(stock_symbols)


# clean stock symbols from here


#Library####
library(tidyverse) #general
library(xml2) #read_html()
library(rvest) #html_nodes()
library(htmltab) #htmltab()

# list of all stock symbols traded
#list of symbols without working directory defined
#nyse <- read.csv("~/Stock Average Price Projection/nyse.csv", stringsAsFactors=FALSE)
#amex <- read_csv("~/Stock Average Price Projection/amex.csv", skip = 1)
#nasdaq <- read.csv("~/Stock Average Price Projection/nasdaq.csv", stringsAsFactors=FALSE)

#Stock Symbol List####
#list of symbols with working directory defined
nyse <- read.csv("nyse.csv", stringsAsFactors=FALSE)
amex <- suppressWarnings(read_csv("amex.csv", skip = 1))
nasdaq <- read.csv("nasdaq.csv", stringsAsFactors=FALSE)

stocks <- rbind(nasdaq, nyse)
names(stocks)[8] <- "Summary Quote"
stocks <- stocks[1:8]
amex <- amex[1:8]

#all stock symbols
stocks <- rbind(stocks, amex)
# finding stock price
stocks <- stocks[,c(1,3)]
stocks$LastSale <- suppressWarnings(as.numeric(as.character(stocks$LastSale)))
stocks <- stocks[complete.cases(stocks), ]
#used to remove cheap penny stocks
current_price <- stocks$LastSale <= 9.99
stocks <- cbind(stocks, current_price)
stocks <- stocks[ which(stocks$current_price=='FALSE'),]

#trying to remove repeats with ^ symbol
stocks <- gsub("\\^.*","", stocks$Symbol)
stocks <- unique(stocks)
stocks <- as.data.frame(stocks)

#the above leads to stock symbol list with stocks greater than $9.99 per share####
#stocks <- stocks$Symbol
#stocks <- sort(stocks)
#stocks <- as.data.frame(stocks)
#stocks$stocks <- gsub("\\..*","",stocks$stocks)
#stocks <- unique(stocks$stocks)
#stock_list <- as.list(stocks)
#stocks <- as.data.frame(stocks)

#Financhill recs####
financhill <- function(stock_sign) {
      
      financhill_url <- paste0("https://financhill.com/stock-forecast/", stock_sign, "-stock-prediction")
      url <- read_html(financhill_url)
      
      words <- url %>%
         html_nodes("h4") %>%
         html_text() %>%
         as.data.frame()
      
      rec <- words[c(12),]
      rec <- as.character(rec)
      rec <- unlist(strsplit(rec, split = " ", fixed = TRUE))
      rec <- tail(rec, n=1)
      rec <- as.character(rec)
      rec <- unlist(strsplit(rec, split = "\t", fixed = TRUE))
      rec <- rec[1]
      rec <- unlist(strsplit(rec, split = " ", fixed = TRUE))
      
      buy <- paste0(stock_sign, " is a Buy")
      result <- ifelse(rec == "Buy", print(buy), "")

   }

test_data <- today_stock_price$Symbol[1:15]

# financhill
for (i in test_data){
   
   tryCatch(
      new_element <- financhill(i)
      )
}

#Barchart####
bar_chart_rec <- function(stock_sign){
   
   bar_chart <- function(stock_sign){
      
      url <-  paste0("https://www.barchart.com/stocks/quotes/", stock_sign)
      url <- read_html(url)
      
      words <- url %>%
         html_nodes("a") %>%
         html_text() %>%
         str_squish() %>%
         as.data.frame()
      
      names(words)[1] = "rec"
      
      # removing blank rows
      words1 <- words[!apply(is.na(words) | words == "", 1, all),]
      words1 <- as.data.frame(words1)
      
      strong_buy <- subset(words1$words1, words1$words1=="Strong buy")
      strong_buy <- as.character(strong_buy)
      
      buy <- subset(words1$words1, words1$words1=="buy")
      buy <- as.character(buy)
      
      weak_buy <- subset(words1$words1, words1$words1=="Weak buy")
      weak_buy <- as.character(weak_buy)
      
      hold <- subset(words1$words1, words1$words1=="hold")
      hold <- as.character(hold)
      
      weak_sell <- subset(words1$words1, words1$words1=="Weak sell")
      weak_sell <- as.character(weak_sell)
      
      sell <- subset(words1$words1, words1$words1=="sell")
      sell <- as.character(sell)
      
      strong_sell <- subset(words1$words1, words1$words1=="Strong sell")
      strong_sell <- as.character(strong_sell)
      
      opinion <- rbind(strong_buy, buy, all = TRUE)
      opinion <- rbind(opinion, weak_buy, all = TRUE)
      opinion <- rbind(opinion, hold, all=TRUE)
      opinion <- rbind(opinion, weak_sell, all = TRUE)
      opinion <- rbind(opinion, sell, all = TRUE)
      opinion <- rbind(opinion, strong_sell, all = TRUE)
      
      opinion <- as.data.frame(opinion)
      
      opinion <- opinion %>% 
         filter(!grepl('TRUE', opinion$V1))
      
      print("Barchart Recommendation")
      ifelse(opinion == "Strong buy" | opinion == "Buy", print(opinion), print(""))
      
      #print(opinion)
      
   }
   stock_analysis <- function(stock_sign){
      
      print(stock_sign)
      ifelse(bar_chart(stock_sign) == "Strong buy" , print(bar_chart(stock_sign)), print(""))
      ifelse(bar_chart(stock_sign) == "Buy" , print(bar_chart(stock_sign)), print(""))
      
   }
   
   stock_analysis(stock_sign)
}

for (i in stock_list){
   new_element <- bar_chart_rec(i)
   tryCatch(
   list_[[length(list_) + 1]] <- new_element
   )
}
