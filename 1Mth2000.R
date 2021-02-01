# 1 month stock volatility
rm(list = ls())

setwd("~/Stock Average Price Projection")

#Library####
library(tidyverse) #general
library(xml2) #read_html()
library(rvest) #html_nodes()
library(htmltab) #htmltab()

#Stock Symbols####
# finding list of all stock symbols
nyse <- read.csv("nyse.csv", stringsAsFactors=FALSE)
amex <- suppressWarnings(read_csv("amex.csv", skip = 1))
nasdaq <- read.csv("nasdaq.csv", stringsAsFactors=FALSE)

stocks <- rbind(nasdaq, nyse)
names(stocks)[8] <- "Summary Quote"
stocks <- stocks[1:8]
amex <- amex[1:8]

stocks <- rbind(stocks, amex)
stocks <- stocks$Symbol
stocks <- sort(stocks)
stocks <- as.data.frame(stocks)
stocks$stocks <- gsub("\\..*","",stocks$stocks)
stocks <- unique(stocks$stocks)
#stock_list <- as.list(stocks)
stocks <- stocks[1001:2000]

#stock_sign = "baba"
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
   current1 <- current[11,1]
   current1 <- gsub(",","",current1)
   current1 <- suppressWarnings(as.numeric(as.character(current1)))
   current2 <- current[15,1]
   current2 <- gsub(",","",current2)
   current2 <- suppressWarnings(as.numeric(as.character(current2)))
   current <- ifelse(is.na(current1) == TRUE, current2, current1)
   current <- as.data.frame(current)
   current <- as.numeric(as.character(current$current))
   
   # Finding 1 month high and low
   # barchart data
   url = paste0("https://www.barchart.com/stocks/quotes/", stock_sign, "/overview")
   data <- suppressWarnings(htmltab(doc = url, which = 1, header = 0))
   
   # 1 month high
   high <- data[1,3]
   high <- gsub("\\-.*","", high)
   high <- str_replace_all(high, ",", "")
   high <- as.numeric(as.character(high))
   
   # 1 month low
   low <- data[1,2]
   low <- gsub("\\+.*","", low)
   low <- str_replace_all(low, ",", "")
   low <- suppressWarnings(as.numeric(as.character(low)))
   
   # 1 month high low difference
   dif = high - low
   
   # number gain
   pot_prof = high - current
   # percent gain
   pot_prof_perc = (pot_prof/dif)*100
   
   # number loss
   pot_loss = current - low
   # percent loss
   pot_loss_perc = (pot_loss/dif)*100
   
   # profit difference number
   profit_diff_num = pot_prof - pot_loss
   profit_diff_num = round(profit_diff_num, digits=2)
   #profit_diff_num1 = paste0(stock_sign,": $", profit_diff_num, " | Current Price: ", current)
   
   # profit percent difference
   profit_diff_perc = pot_prof_perc - pot_loss_perc
   profit_diff_perc = round(profit_diff_perc, digits=2)
   #profit_diff_perc1 = paste0(stock_sign, ": ", profit_diff_perc, "% | Current Price: $", current)
   
   # potential profit
   #Greater than 80% potential profit
   #ifelse(profit_diff_perc > 80, print(profit_diff_perc1), "")
   #Greater than $15 potential profit
   #ifelse(profit_diff_num > 15, print(profit_diff_num1), "")
   
   results = paste0(stock_sign," | Current Price: ", current, " | Profit ($) = $", profit_diff_num, " | Profit (%) = ", profit_diff_perc, "%")
   
   # both greater than 80% and $10 profit potential
   ifelse(profit_diff_num > 1 & profit_diff_perc > 80, print(results), "")
   
}

#month_profit_range("baba")

# Finding Stocks With Potential Profit (> 75% and/or > $5)
for ( i in stocks ){
   
   tryCatch(
      
      month_profit_range(i), error = function(e){})
   
}

