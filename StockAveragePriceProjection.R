# Stock average price projection template
# goal: take max, min, and mean and of puts and calls
# create % metrix which shows the projected growth rate for each stock
# puts and calls will show bullish or bearish sentiminet regarding stock

# 52 week stock data

#library####

library(tidyverse) #general
library(xml2) #read_html()
library(rvest) #html_nodes()
library(htmltab) #htmltab()

#Stock Symbols####
# finding list of all stock symbols
data <- function(){
   
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

rm(other)
rm(nasdaq)
rm(stock_names1)
rm(stock_names2)

names(stock_names)[1] <- "stock_symbols"

data <- as.character(stock_names$stock_symbols)
data <- gsub("\\..*","", data)
data <- unique(data)
}

# data function saves stock symbol list
data <- data()

#Profit Potential Function####
# based on range between highs and lows in 52 week range
profit_potential <- function(stock_sign){

   yahoo_url <- paste0("https://finance.yahoo.com/quote/", stock_sign, "?p=", stock_sign)

   stock_data <- suppressWarnings(htmltab(doc = yahoo_url, which = 1, header = 0))
   stock_data <- stock_data[6,2]
   stock_data <- strsplit(stock_data, " ")
   stock_data <- data.frame(prices=unlist(stock_data))

# min from 52 week range
   min <- stock_data[1,1]
   min <- sub(" .*", "", min)
   min <- suppressWarnings(as.numeric(min))

# max from 52 week range
   max <- stock_data[3,1]
   max <- sub(" .*", "", max)
   max <- suppressWarnings(as.numeric(max))

# current price
   current <- read_html(yahoo_url)

   current <- current %>%
      html_nodes("span") %>%
      html_text() %>%
      as.data.frame()

   current <- current[!apply(is.na(current) | current == "", 1, all),]
   current <- as.data.frame(current)
   current <- current[grep("[[:digit:]]", current$current), ]
   current <- as.data.frame(current)
   current <- current[!grepl("W", current$current),]
   current <- as.data.frame(current)
   current1 <- current[11,1]
   current1 <- gsub(",","",current1)
   current1 <- suppressWarnings(as.numeric(as.character(current1)))
   current2 <- current[15,1]
   current2 <- gsub(",","",current2)
   current2 <- suppressWarnings(as.numeric(as.character(current2)))
   current <- ifelse(is.na(current1) == TRUE, current2, current1)
   current <- as.data.frame(current)
   current <- as.numeric(as.character(current$current))

# calculating % and # above and below stock based on min and max
# total range
   total = max - min

# UPPER HALF
# top as number
   top = max - current
   top = round(top, digits=2)
   top1 <- paste0("$", top)

# top as %
   top_perc = (top/total)*100
   top_perc = round(top_perc, digits=2)
   top_perc1 <- paste0(top_perc, "%")

# LOWER HALF
# bottom as number
   bottom = current - min
   bottom = round(bottom, digits=2)

# as percent
   bottom_perc = (bottom/total)*100
   bottom_perc = round(bottom_perc, digits=2)

# Profit Range
   print(stock_sign)
   ifelse(bottom_perc > top_perc, "Above Average", "Below Average")
   print("Potential Growth")
   ifelse(top > 75, "> $75 growth", " ")
   ifelse(top_perc > 75, "> 75% growth", " ")
   print(top1)
   print(top_perc1)
   print(" ")

}

#Loop of Profit Potential Function####
for ( i in data ){
   
   tryCatch(
     profit_potential(i), error = function(e){})
   # including ", error = function(e){}" is optional
   
}

#% and $ Gain Range Stocks####
# choose range of profit levels (% and $) for a specific stock
only_text <- function(stock_sign, profit_gap_percent, profit_gap_number){
   yahoo_url <- paste0("https://finance.yahoo.com/quote/", stock_sign, "?p=", stock_sign)

   stock_data <- suppressWarnings(htmltab(doc = yahoo_url, which = 1, header = 0))
   stock_data <- stock_data[6,2]
   stock_data <- strsplit(stock_data, " ")
   stock_data <- data.frame(prices=unlist(stock_data))

# min from 52 week range
   min <- stock_data[1,1]
   min <- sub(" .*", "", min)
   min <- suppressWarnings(as.numeric(min))

# max from 52 week range
   max <- stock_data[3,1]
   max <- sub(" .*", "", max)
   max <- suppressWarnings(as.numeric(max))

# current price
   current <- read_html(yahoo_url)

   current <- current %>%
      html_nodes("span") %>%
      html_text() %>%
      as.data.frame()

   current <- current[!apply(is.na(current) | current == "", 1, all),]
   current <- as.data.frame(current)
   current <- current[grep("[[:digit:]]", current$current), ]
   current <- as.data.frame(current)
   current <- current[!grepl("W", current$current),]
   current <- as.data.frame(current)
   current1 <- current[11,1]
   current1 <- gsub(",","",current1)
   current1 <- suppressWarnings(as.numeric(as.character(current1)))
   current2 <- current[15,1]
   current2 <- gsub(",","",current2)
   current2 <- suppressWarnings(as.numeric(as.character(current2)))
   current <- ifelse(is.na(current1) == TRUE, current2, current1)
   current <- as.data.frame(current)
   current <- as.numeric(as.character(current$current))

   rm(current1)
   rm(current2)

# calculating % and # above and below stock based on min and max
# total range
   total = max - min

# UPPER HALF
# top as number
   top = max - current
   #top = round(top, digits=2)
   top1 <- paste0("$", top)

# top as %
   top_perc = (top/total)*100
   #top_perc = round(top_perc, digits=2)
   top_perc1 <- paste0(top_perc, "%")

# LOWER HALF
# bottom as number
   bottom = current - min
   #bottom = round(bottom, digits=2)

# as percent
   bottom_perc = (bottom/total)*100
   #bottom_perc = round(bottom_perc, digits=2)

# Profit Range
   print(stock_sign)
   # ifelse(bottom_perc > top_perc, "Above Average", "Below Average")
# print("Potential Growth")
# print(top1)
   # ifelse(top > 75, print("> $75 growth"), " ")
# print(top_perc1)
   # ifelse(top_perc > 75, print("> 75% growth"), " ")
   profit_gap_percent <- as.numeric(as.character(profit_gap_percent))
   profit_gap_number <- as.numeric(as.character(profit_gap_number))
   # percent profit text
   profit_perc_text <- paste0("more than ", profit_gap_percent, "% profit")
   # dollar profit text 
   profit_num_text <- paste0("more than $", profit_gap_number, " profit")
   both <- paste0(profit_perc_text, " and ", profit_num_text)
   ifelse(top_perc > profit_gap_percent, print(profit_perc_text), "")
   ifelse(top > profit_gap_number, print(profit_num_text), "")
   ifelse(top_perc > profit_gap_percent & top > profit_gap_number, print(both), "")

}

#only_text("hii", 10, 25)

#example loop of gains####

for ( i in test_data ){
   
   tryCatch(
      only_text(i, 10, 10), error = function(e){})
   
}
