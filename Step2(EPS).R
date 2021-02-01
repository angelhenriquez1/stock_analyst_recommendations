# Creating 3 stage formula using PE, EPS, and double profit range
# this is the EPS one
rm(list = ls())

#Library####
library(tidyverse) #general
library(xml2) #read_html()
library(rvest) #html_nodes()
library(htmltab) #htmltab()

setwd("~/Stock Average Price Projection")
# list of stocks already cleaned by positive PE ratio
stock_tickers <- read_csv("last_dataset.csv", col_names = FALSE)
stock_tickers <- gsub('"',"", stock_tickers$X1)
stock_tickers <- as.data.frame(stock_tickers)
stock_tickers <- sub(".*? ", "", stock_tickers$stock_tickers)
stock_tickers <- as.data.frame(stock_tickers)

stock_tickers <- subset(stock_tickers, stock_tickers != "NA" & stock_tickers != "")

positive_eps <- function(stock_sign){
   
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

for ( i in stock_tickers$stock_tickers ){
   
   tryCatch(
      
      positive_eps(i), error = function(e){})
   
}



