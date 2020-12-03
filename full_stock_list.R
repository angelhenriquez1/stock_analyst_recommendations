# list of stock symbols

library(tidyverse)
library(htmltab)
library(rvest)
library(xml2) 

#Stock Symbols####
# finding list of all stock symbols
nyse <- read.csv("nyse.csv", stringsAsFactors=FALSE)
amex <- read_csv("amex.csv", skip = 1)
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
stock_list <- as.list(stocks)


#Old Stock Symbol List Code####

   other <- suppressWarnings(read_delim("http://ftp.nasdaqtrader.com/dynamic/SymDir/otherlisted.txt",
                                        "|", escape_double = FALSE, trim_ws = TRUE))
   
   other <- subset(other, other$ETF != "Y")
   other <- other[- grep("ETN", other$`Security Name`),]
   
   nasdaq <- suppressWarnings(read_delim("http://ftp.nasdaqtrader.com/dynamic/SymDir/nasdaqtraded.txt",
                                         "|", escape_double = FALSE, trim_ws = TRUE))
   
   nasdaq <- subset(nasdaq, nasdaq$ETF != "Y")
   nasdaq <- nasdaq[- grep("ETN", nasdaq$`Security Name`),]
   
   stock_names1 <- other[1]
   names(stock_names1)[1] <- "stock_symbols"
   stock_names2 <- nasdaq[1]
   names(stock_names2)[1] <- "stock_symbols"
   stock_names <- rbind(stock_names1, stock_names2)
   stock_names <- gsub("\\$.*","", stock_names$stock_symbols)
   stock_names <- as.data.frame(as.character(stock_names))
   
   names(stock_names)[1] <- "stock_symbols"
   
   stock_names$stock_symbols <- as.character(stock_names$stock_symbols)
   stock_names$stock_symbols <- gsub("\\..*","", stock_names$stock_symbols)
   stock_names <- unique(stock_names$stock_symbols)

   #this is my final dataset
   stock_names1 <- as.data.frame(stock_names)
   stock_names1 <- as.character(stock_names)
   
   stock_names_test <- stock_names1[1:5]

   typeof(stock_names_test)
   
   # current price
   stock_names_test1 <- paste0("https://finance.yahoo.com/quote/", stock_names_test, "?p=", stock_names_test)
   stock_names_test1 <- paste0("https://finance.yahoo.com/quote/", stock_names_test, "?p=", stock_names_test)
   
   for (i in i){

         print(stock_names)
      
   }
   
   
   
   
   
   
   
   
   
   #view(stock_names_test1)
   stock_names_test2 <- as.data.frame(stock_names_test1)
   
   stock_names_test3 <- as.character(stock_names_test2$stock_names_test1)
   stock_names_test4 <- as.data.frame(stock_names_test3)
   
   view(stock_names_test1)
   
   
   #stock_names_test2 <- as.data.frame.character((stock_names_test1))
   stock_names_test2
   
   stock_names_test3 <- read_html(stock_names_test2[1])

   a <- as.list(stock_names_test3)

   current <- read_html(a$node)
   
   
   current <- current %>%
      html_nodes("span") %>%
      html_text() %>%
      as.data.frame()
   
names(stock_names_test)[1] <- "stuff"

stock_names_test$stuff2 <- paste0("https://finance.yahoo.com/quote/", stock_names_test$stuff, "?p=", stock_names_test$stuff)


stock_names_test$stuff3 <- stock_names_test$stuff2

stock_names_test$stuff3 <- read_html(stock_names_test$stuff2)


?read_html

# current price
yahoo_url <- paste0(stock_sign, "?p=", stock_sign)











