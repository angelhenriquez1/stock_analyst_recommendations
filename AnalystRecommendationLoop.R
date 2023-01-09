#Retrieve Analyst Recommendations####

# Package libraries
library(tidyverse)
library(rvest)
library(xml2)
library(data.table)

#Error handling
# error = function(e){NA}
#Clearing data
rm(list = ls())
# Setting working directory with nyse, amex, and nasdaq
setwd("~/Stock Average Price Projection")

#Retrieving Stock Data From CSV####

# NYSE Data from CSV file
nyse <- as.data.frame(read.csv("nyse.csv", stringsAsFactors=FALSE))
# Nasdaq Data from CSV file
nasdaq <- as.data.frame(read.csv("nasdaq.csv", stringsAsFactors=FALSE))
# Amex Data from CSV file
amex <- as.data.frame(read_csv("amex.csv"))
# Matching Amex column names
colnames(amex)[3] <- "Last.Sale"
colnames(amex)[4] <- "Net.Change"
colnames(amex)[5] <- "X..Change"
colnames(amex)[6] <- "Market.Cap"
colnames(amex)[8] <- "IPO.Year"

# Merging Stock Data
all_stock_data <- merge.data.frame(nyse,nasdaq, all=TRUE)
all_stock_data <- merge.data.frame(all_stock_data,amex, all=TRUE)

all_stock_data$Last.Sale <- gsub(".*\\$","",all_stock_data$Last.Sale)
all_stock_data$Last.Sale <- as.numeric(all_stock_data$Last.Sale)

all_stock_data_subset <- subset(all_stock_data, all_stock_data$Country == "United States" 
                         & all_stock_data$Last.Sale > 7.5
                         & all_stock_data$Volume > 50000)

#Stock Subsets####

all_stock_data_subset <- unique(gsub("\\^.*", "", all_stock_data_subset$Symbol))

rm("amex")
rm("nasdaq")
rm("nyse")

print("start")

#Webscraping Code####

# Creating list at beginning of code (Run 2 time)
stockRec <- data.frame(matrix(ncol = 2, nrow = 1))
colnames(stockRec) <- c('BarChart', 'Stock Sign')

for (stock_sign in all_stock_data_subset) {
   
   bar_chart <- tryCatch({bar_chart <- read_html(paste0("https://www.barchart.com/stocks/quotes/", stock_sign)) %>%
      html_nodes(".rating") %>%
      html_text() %>%
      str_trim()}, error = function(e){NA})
   
   # Combining data into row
   stock_ranking <- data.frame(matrix(c(bar_chart, stock_sign), ncol = 2, nrow = 1))
   
   # matching column names for rbind function to work
   names(stock_ranking) <- names(stockRec)
   
   # Adding row of stock data
   stockRec <- rbind(stockRec, stock_ranking)
   
}

print("end of barchart")

# check to make sure only buy stocks are included for bar chart
stocksWithData <- subset(stockRec, stockRec$BarChart != stockRec$`Stock Sign`)
unique_stock_recommendation_types <- as.data.frame(unique(stocksWithData$BarChart))

barChart_stocks <- subset(stockRec$`Stock Sign`, 
                              stockRec$BarChart != "Hold" & 
                              stockRec$BarChart != "Moderate Sell" & 
                              stockRec$BarChart != "Strong Sell")

# Creating list at beginning of code (Run 3 time)
stockRec2 <- data.frame(matrix(ncol = 2, nrow = 1))
colnames(stockRec2) <- c('Zacks', 'Stock Sign')

for (stock_sign in barChart_stocks) {
   
   zacks_rank <- tryCatch({zacks_rank <- read_html(paste0("https://www.zacks.com/stock/research/", stock_sign, "/stock-style-scores")) %>%
      html_nodes("p.rank_view") %>%
      html_text() %>%
      str_trim() %>%
      .[1]},
      error = function(e){NA})
   
   # Combining data into row
   stock_ranking2 <- data.frame(matrix(c(zacks_rank, stock_sign), ncol = 2, nrow = 1))
   
   # matching column names for rbind function to work
   names(stock_ranking2) <- names(stockRec2)
   
   # Adding row of stock data
   stockRec2 <- rbind(stockRec2, stock_ranking2)
   
}

print("end of zacks")

# cleaning data
stockRec2$Zacks <- str_trim(stockRec2$Zacks)
stockRec2$Zacks <- gsub("1-|2-|3-|4-|5-|of|1|2|3|4|5","", stockRec2$Zacks)
stockRec2$Zacks <- gsub("Se", "Sell", stockRec2$Zacks)
stockRec2$Zacks <- str_squish(stockRec2$Zacks)

# check to make sure only buy stocks are included for bar chart
stocksWithData <- subset(stockRec2, stockRec2$Zacks != stockRec2$`Stock Sign`)
unique_stock_recommendation_types <- as.data.frame(unique(stocksWithData$Zacks))

#subset removing non-recommended stocks with data present
zacks_buy_stocks <- subset(stockRec2$`Stock Sign`, 
                              stockRec2$Zacks != "Hold" & 
                              stockRec2$Zacks != "Sellll" & 
                              stockRec2$Zacks != "Strong Sellll")

# Creating list at beginning of code (Run 1 time)
stockRec3 <- data.frame(matrix(ncol = 2, nrow = 1))
colnames(stockRec3) <- c('Financhill', 'Stock Sign')

for (stock_sign in zacks_buy_stocks) {
   
   #1 Financhill
   financhill_rec <- tryCatch({financhill_rec <- read_html(paste0("https://financhill.com/stock-forecast/", stock_sign, "-stock-prediction")) %>%
      html_nodes("div.stock-score-text") %>%
      html_text() %>%
      str_trim() %>%
      .[1]},
      error = function(e){NA})
   
   # Combining data into row
   stock_ranking3 <- data.frame(matrix(c(financhill_rec, stock_sign), ncol = 2, nrow = 1))
   
   # matching column names for rbind function to work
   names(stock_ranking3) <- names(stockRec3)
   
   # Adding row of stock data
   stockRec3 <- rbind(stockRec3, stock_ranking3)
   
}

print("end of financhill")

# check to make sure only buy stocks are included for bar chart
stocksWithData <- subset(stockRec3, stockRec3$Financhill != stockRec3$`Stock Sign`)
unique_stock_recommendation_types <- as.data.frame(unique(stocksWithData$Financhill))

# cleaning data
financhill_buy_stocks <- subset(stockRec3$`Stock Sign`, stockRec3$Financhill != "Sell")

# Creating list at beginning of code (Run 4 time)
stockRec4 <- data.frame(matrix(ncol = 2, nrow = 1))
colnames(stockRec4) <- c('MarketWatch', 'Stock Sign')

for (stock_sign in financhill_buy_stocks) {
   
   marketwatch <- tryCatch({marketwatch <- read_html(paste0("https://www.marketwatch.com/investing/stock/", stock_sign, "/analystestimates")) %>%
      html_nodes("td.table__cell.w25") %>%
      html_text()%>%
      str_trim() %>%
      .[1]}, error = function(e){NA})
   
   # Combining data into row
   stock_ranking4 <- data.frame(matrix(c(marketwatch, stock_sign), ncol = 2, nrow = 1))
   
   # matching column names for rbind function to work
   names(stock_ranking4) <- names(stockRec4)
   
   # Adding row of stock data
   stockRec4 <- rbind(stockRec4, stock_ranking4)
   
}

print("end of marketwatch")

# check to make sure only buy stocks are included for bar chart
stocksWithData <- subset(stockRec4, stockRec4$MarketWatch != stockRec4$`Stock Sign`)
unique_stock_recommendation_types <- as.data.frame(unique(stocksWithData$MarketWatch))

# subset of buy stocks
marketwatch_buy_stocks <- subset(stockRec4$`Stock Sign`, 
                                 stockRec4$MarketWatch != "Sell" &
                                 stockRec4$MarketWatch != "Hold" &
                                 stockRec4$MarketWatch != "Underweight")

# Creating list at beginning of code (Run 5 time)
stockRec5 <- data.frame(matrix(ncol = 2, nrow = 1))
colnames(stockRec5) <- c('CNN', 'Stock Sign')

for (stock_sign in marketwatch_buy_stocks) {
   
   CNN <- tryCatch({CNN <- read_html(paste0("http://markets.money.cnn.com/research/quote/forecasts.asp?symb=", stock_sign)) %>%
      html_nodes("p strong") %>%
      html_text()}, error = function(e){NA})

   # Combining data into row
   stock_ranking5 <- data.frame(matrix(c(CNN, stock_sign), ncol = 2, nrow = 1))
   
   # matching column names for rbind function to work
   names(stock_ranking5) <- names(stockRec5)
   
   # Adding row of stock data
   stockRec5 <- rbind(stockRec5, stock_ranking5)
   
}

print("end of CNN")

# check to make sure only buy stocks are included for bar chart
stocksWithData <- subset(stockRec5, stockRec5$CNN != stockRec5$`Stock Sign`)
unique_stock_recommendation_types <- as.data.frame(unique(stocksWithData$CNN))

# subset of buy stocks
CNN_buy_stocks <- subset(stockRec5$`Stock Sign`, 
                              stockRec5$CNN != "Hold" &
                              stockRec5$CNN != "Sell")

# Creating list at beginning of code (Run 6 time)
stockRec6 <- data.frame(matrix(ncol = 2, nrow = 1))
colnames(stockRec6) <- c('MarketsInsider', 'Stock Sign')

for (stock_sign in CNN_buy_stocks) {
   
   markets_insider <- tryCatch({markets_insider <- read_html(paste0("https://markets.businessinsider.com/stocks/", stock_sign, "-stock")) %>%
      html_nodes("span.analysis-chart__value") %>%
      html_text() %>%
      str_trim()},
      error = function(e){NA})
   
   markets_insider <- tryCatch({markets_insider <- paste0(round((as.numeric(markets_insider[2]))/(as.numeric(markets_insider[1])),2)*100,"% Buy Rating by ", as.numeric(markets_insider[1]), " analysts.")}, error = function(e){NA})
   
   # Combining data into row
   stock_ranking6 <- data.frame(matrix(c(markets_insider, stock_sign), ncol = 2, nrow = 1))
   
   # matching column names for rbind function to work
   names(stock_ranking6) <- names(stockRec6)
   
   # Adding row of stock data
   stockRec6 <- rbind(stockRec6, stock_ranking6)
   
}

print("end of Markets Insider")

# check to make sure only buy stocks are included for bar chart
stocksWithData <- subset(stockRec6, stockRec6$MarketsInsider != stockRec6$`Stock Sign`)
unique_stock_recommendation_types <- as.data.frame(unique(stocksWithData$MarketsInsider))

# cleaning string
stockRec6$MarketsInsider <- as.numeric(gsub("%.*","",stockRec6$MarketsInsider))

# subset of buy stocks
MarketsInsider_buy_stocks <- subset(stockRec6, 
                         stockRec6$MarketsInsider > 74)
MarketsInsider_buy_stocks <- MarketsInsider_buy_stocks[,2]

# Creating list at beginning of code (Run 7 time)
stockRec7 <- data.frame(matrix(ncol = 2, nrow = 1))
colnames(stockRec7) <- c('WalletInvestor', 'Stock Sign')

for (stock_sign in MarketsInsider_buy_stocks) {
   
   #9 Wallet Investor
   wallet_investor <- tryCatch({wallet_investor <- read_html(paste0("https://walletinvestor.com/stock-forecast/", stock_sign, "-stock-prediction")) %>%
      html_nodes("strong") %>%
      html_text()%>%
      str_trim()},
      error = function(e){NA})
   
   wallet_investor <- tryCatch({wallet_investor <- paste0(wallet_investor[3], " is a ", wallet_investor[5], " long term (1 year) investment.")}, error = function(e){NA})
   # Combining data into row
   stock_ranking7 <- data.frame(matrix(c(wallet_investor, stock_sign), ncol = 2, nrow = 1))
   
   # matching column names for rbind function to work
   names(stock_ranking7) <- names(stockRec7)
   
   # Adding row of stock data
   stockRec7 <- rbind(stockRec7, stock_ranking7)
   
}

print("end of Wallet Investor")

# check to make sure only buy stocks are included for bar chart
stocksWithData <- subset(stockRec7, stockRec7$WalletInvestor != stockRec7$`Stock Sign`)
unique_stock_recommendation_types <- as.data.frame(unique(stocksWithData$WalletInvestor))

WalletInvestor_buy_stocks <- stockRec7[!stockRec7$WalletInvestor %like% "bad long term", ]
WalletInvestor_buy_stocks <- WalletInvestor_buy_stocks[!WalletInvestor_buy_stocks$WalletInvestor %like% "not so good", ]

# Creating list at beginning of code (Run 8 time)
stockRec8 <- data.frame(matrix(ncol = 2, nrow = 1))
colnames(stockRec8) <- c('YahooFinance', 'Stock Sign')

for (stock_sign in WalletInvestor_buy_stocks$`Stock Sign`) {
   
   #8 Yahoo Finance
   yahoo_finance <- tryCatch({yahoo_finance <- read_html(paste0("https://finance.yahoo.com/quote/", stock_sign, "?p=", stock_sign)) %>%
      html_nodes("div#fr-val-mod") %>%
      html_text()
   },
   error = function(e){NA})
   
   yahoo_finance <- tryCatch({yahoo_finance <- gsub(".*XX.XX","",yahoo_finance)}, error = function(e){NA})
   
   yahoo_finance <- tryCatch({yahoo_finance <- gsub("Subscribe.*","",yahoo_finance)}, error = function(e){NA})

   # Combining data into row
   stock_ranking8 <- data.frame(matrix(c(yahoo_finance, stock_sign), ncol = 2, nrow = 1))
   
   # matching column names for rbind function to work
   names(stock_ranking8) <- names(stockRec8)
   
   # Adding row of stock data
   stockRec8 <- rbind(stockRec8, stock_ranking8)
   
}

print("end of Yahoo Finance")

# check to make sure only buy stocks are included for bar chart
stocksWithData <- subset(stockRec8, stockRec8$YahooFinance != stockRec8$`Stock Sign`)
unique_stock_recommendation_types <- as.data.frame(unique(stocksWithData$YahooFinance))

# Delete non-alphanumeric
stockRec8$YahooFinance <- str_replace_all(stockRec8$YahooFinance, "[^[:alnum:]]", "")
stockRec8$YahooFinance <- gsub("[0-9]+","",stockRec8$YahooFinance)
stockRec8$YahooFinance <- gsub("EstReturn","",stockRec8$YahooFinance)

# check to make sure only buy stocks are included for bar chart
stocksWithData <- subset(stockRec8, stockRec8$YahooFinance != stockRec8$`Stock Sign`)
unique_stock_recommendation_types <- as.data.frame(unique(stocksWithData$YahooFinance))

#Subset of final buy stocks
Yahoo_Finance_buy_stocks <- subset(stockRec8$`Stock Sign`, 
                                   stockRec8$YahooFinance != "NearFairValue" &
                                   stockRec8$YahooFinance != "Overvalued")

#Data frame of final buy stocks
final_stock_list<- as.data.frame(Yahoo_Finance_buy_stocks)
