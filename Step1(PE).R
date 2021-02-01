# Creating 3 stage formula using PE, EPS, and double profit range
# this is the PE one
rm(list = ls())

#setting working directory
setwd("~/Stock Average Price Projection")

#Library####
library(tidyverse) #general
library(xml2) #read_html()
library(rvest) #html_nodes()
library(htmltab) #htmltab()

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

# list of stocks already cleaned by profit being at least 2.5 times the previous low
#stock_symbols <- read_csv("(today)stock_symbols.csv", col_names = FALSE)
#stock_symbols <- stock_symbols[,1]

#stock_symbols <- gsub('"',"", stock_symbols$X1)
#stock_symbols <- as.data.frame(stock_symbols)
#stock_symbols <- sub(".*? ", "", stock_symbols$stock_symbols)
#stock_symbols <- as.data.frame(stock_symbols)

# going to find stocks with positive PE ratios

positive_pe <- function(stock_sign){
   
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
   current1 <- current[23,1]
   current1 <- gsub(",","",current1)
   current1 <- suppressWarnings(as.numeric(as.character(current1)))
   current2 <- current[27,1]
   current2 <- gsub(",","",current2)
   current2 <- suppressWarnings(as.numeric(as.character(current2)))
   current <- ifelse(is.na(current1) == TRUE, current2, current1)
   current <- as.data.frame(current)
   current <- as.numeric(as.character(current$current))
   
   # determine if pe positive
   print(ifelse(current > 0, stock_sign, ""))
   
}


for ( i in stocks$stocks ){
   
   tryCatch(
      
      positive_pe(i), error = function(e){})
   
}



