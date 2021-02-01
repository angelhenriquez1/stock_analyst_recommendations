# library
library(tidyverse) #general
library(xml2) #read_html()
library(rvest) #html_nodes()
library(htmltab) #htmltab()

#Stock Ticker List####
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

#52 Week Profit Range####
# checking if profits have doubled
week52 <- function(stock_sign){
   
   # current price
   yahoo_url <- paste0("https://finance.yahoo.com/quote/", stock_sign, "?p=", stock_sign)
   
   current <- read_html(yahoo_url)
   
   current <- current %>%
      html_nodes("span") %>%
      html_text() %>%
      as.data.frame()
   
   current <- as.data.frame(current[!apply(is.na(current) | current == "", 1, all),])
   current <- as.data.frame(current[grep("[[:digit:]]", current$current), ])
   current <- as.data.frame(current[!grepl("W", current$current),])
   current1 <- current[11,1]
   current1 <- gsub(",","",current1)
   current1 <- suppressWarnings(as.numeric(as.character(current1)))
   current2 <- current[15,1]
   current2 <- gsub(",","",current2)
   current2 <- suppressWarnings(as.numeric(as.character(current2)))
   current <- ifelse(is.na(current1) == TRUE, current2, current1)
   current <- as.data.frame(current)
   current <- as.numeric(as.character(current$current))
   
   # Finding 52 week high and low
   # barchart data
   url = paste0("https://www.barchart.com/stocks/quotes/", stock_sign, "/overview")
   data <- suppressWarnings(htmltab(doc = url, which = 1, header = 0))
   # 52 week high
   high <- data[3,3]
   high <- gsub("\\-.*","", high)
   high <- str_replace_all(high, ",", "")
   high <- suppressWarnings(as.numeric(as.character(high)))
   
   # 52 week low
   low <- data[3,2]
   low <- gsub("\\+.*","", low)
   low <- str_replace_all(low, ",", "")
   low <- suppressWarnings(as.numeric(as.character(low)))
   
   ifelse(high > (2.5*low), print(stock_sign), "")
   
}

#stock_listing <- as.list(stocks)
# Finding Stocks With Potential Profit (> 70% and/or > $10)
for ( i in stocks$stocks ){
   
   tryCatch(
      
      week52(i), error = function(e){})
   
}
