# 3 month stock volatility
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

#3 Month Profit Range####
qrt_yr_profit_range <- function(stock_sign){

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
   
   # Finding 3 month high and low
   # barchart data
   url = paste0("https://www.barchart.com/stocks/quotes/", stock_sign, "/overview")
   data <- suppressWarnings(htmltab(doc = url, which = 1, header = 0))
   
   # 3 month high
   high <- data[2,3]
   high <- gsub("\\-.*","", high)
   high <- str_replace_all(high, ",", "")
   high <- suppressWarnings(as.numeric(as.character(high)))
   
   # 3 month low
   low <- data[2,2]
   low <- gsub("\\+.*","", low)
   low <- str_replace_all(low, ",", "")
   low <- suppressWarnings(as.numeric(as.character(low)))
   
   # 3 month high low difference
   dif = high - low
   
   # gain (number)
   pot_prof = high - current
   # gain (percent)
   pot_prof_perc = (pot_prof/dif)*100
   
   # loss (number)
   pot_loss = current - low
   # loss (percent)
   pot_loss_perc = (pot_loss/dif)*100
   
   # profit difference (number)
   profit_diff_num = pot_prof - pot_loss
   profit_diff_num = round(profit_diff_num, digits=2)
   profit_diff_num1 = paste0(stock_sign," has $", profit_diff_num, " profit potential.")
  
   # profit difference (percent)
   profit_diff_perc = pot_prof_perc - pot_loss_perc
   profit_diff_perc = round(profit_diff_perc, digits=2)
   profit_diff_perc1 = paste0(stock_sign, " has ", profit_diff_perc, "% profit potential.")
   print(stock_sign)
  
   # potential profit
   #Greater than 90% and $10 profit difference
   ifelse(profit_diff_perc > 10 & profit_diff_num > 10, print(profit_diff_perc1), "")
   
}

# insert ticker symbol for specific stock profit potential
qrt_yr_profit_range("ttd")

# Finding profitable Stocks (> 90% profitability and > $10 stock price profit range) from 1000+ stocks

for ( i in stocks$stocks ){
   
   tryCatch(
      
      qrt_yr_profit_range(i), error = function(e){})
   
}
