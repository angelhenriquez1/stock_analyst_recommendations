# 1 MONTH STOCK PROFIT RANGES
rm(list = ls())
# list of stock symbols

library(tidyverse)
library(htmltab)
library(rvest)
library(xml2) 

#Stock Symbols####
# finding list of all stock symbols
nyse <- read.csv("nyse.csv", stringsAsFactors=FALSE)
amex <- suppressWarnings(read_csv("amex.csv", skip = 1))
nasdaq <- read.csv("nasdaq.csv", stringsAsFactors=FALSE)

# merging stocks
stocks <- rbind(nasdaq, nyse)
names(stocks)[8] <- "Summary Quote"
stocks <- stocks[1:8]
amex <- amex[1:8]

stocks <- rbind(stocks, amex)
stocks <- stocks$Symbol
stocks <- sort(stocks)
stocks <- as.data.frame(stocks)
stocks$stocks <- gsub("\\..*","",stocks$stocks)
stocks <- as.data.frame(stocks)
stocks$stocks <- gsub("\\^.*","",stocks$stocks)
stocks <- unique(stocks$stocks)

rm("amex")
rm("nasdaq")
rm("nyse")

#1 Month Profit Range####
month_profit_range <- function(stock_sign){

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
   current1 <- current[12,1]
   current1 <- gsub(",","",current1)
   current1 <- suppressWarnings(as.numeric(as.character(current1)))
   current2 <- current[16,1]
   current2 <- gsub(",","",current2)
   current2 <- suppressWarnings(as.numeric(as.character(current2)))
   current <- ifelse(is.na(current1) == TRUE, current2, current1)
   current <- as.data.frame(current)
   current <- as.numeric(as.character(current$current))
   
   # barchart data
   url = paste0("https://www.barchart.com/stocks/quotes/", stock_sign, "/overview")
   data <- suppressWarnings(htmltab(doc = url, which = 1, header = 0))
   
   # 1 month stock price high
   high <- data[1,3]
   high <- gsub("\\-.*","", high)
   high <- str_replace_all(high, ",", "")
   high <- as.numeric(as.character(high))
   
   # 1 month stock price low
   low <- data[1,2]
   low <- gsub("\\+.*","", low)
   low <- str_replace_all(low, ",", "")
   low <- suppressWarnings(as.numeric(as.character(low)))
   
   # 1 month high low difference
   dif = high - low
   
   # profit (number)
   pot_prof = high - current
   # profit (percent)
   pot_prof_perc = (pot_prof/dif)*100
   
   # loss (number)
   pot_loss = current - low
   # loss (percent)
   pot_loss_perc = (pot_loss/dif)*100
   
   # profit - loss difference (number)
   profit_diff_num = pot_prof - pot_loss
   profit_diff_num = round(profit_diff_num, digits=2)

   # profit - loss difference (percent)
   profit_diff_perc = pot_prof_perc - pot_loss_perc
   profit_diff_perc = round(profit_diff_perc, digits=2)
   
   results = paste0(stock_sign," | Current Price: ", current, " | Profit ($) = $", profit_diff_num, " | Profit (%) = ", profit_diff_perc, "%")
   # Stocks with both greater than 80% and $10 profit potential
   ifelse(profit_diff_num > 10 & profit_diff_perc > 85, print(results), "")
   
}

# Loop for 1000+ stock tickers 
for ( i in stocks$stocks ){
tryCatch( month_profit_range(i), error = function(e){print})
}
