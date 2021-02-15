# Retrieves Analyst Recommendations

library(tidyverse)
library(htmltab)
library(rvest)
library(xml2)

#1 Month Profit Range####
stock_remover <- function(stock_sign){
   
   # current price
   yahoo_url <- paste0("https://finance.yahoo.com/quote/", stock_sign, "?p=", stock_sign)
   
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
   current1 <- as.numeric(current1)
   
   not_stock_of_interest <- paste0(stock_sign, ": Remove")
   
   punct_test <- grepl('[^[:punct:]]', current1)
   
   ifelse(punct_test == FALSE, print(not_stock_of_interest), "")
   
}

stock_recs <- function(stock_sign) {

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
         
         print("Financhill Recommendation")
         rec <- ifelse(rec=="Now", print("No Data"), print(rec))
         
      }
   
   zacks_stock_price <- function(stock_sign) {
      
      stock_sign <- as.character(stock_sign)
      mw_url <- paste0("https://www.zacks.com/stock/quote/", stock_sign)
      stock_rec <- htmltab(doc = mw_url, which = 6, header = 0)
      stock_rec <- stock_rec[1,2]
      stock_rec <- gsub('[[:digit:]]+', '', stock_rec)
      
      mw_url2 <- paste0("https://www.zacks.com/stock/research/", stock_sign, "/industry-comparison")
      stock_avg_rec <- htmltab(doc = mw_url2, which = 4, header = 0)
      stock_avg_rec <- stock_avg_rec[1,2]
      
      print("Zack's Investment")
      print(stock_rec)
      print("Analyst Recommendation (1 = Strong Buy, 5 = Strong Sell)")
      print(stock_avg_rec)
      
   }
   
   finviz_stock_price <- function(stock_sign) {
      
      stock_sign <- as.character(stock_sign)
      fv_url <- paste0("https://finviz.com/quote.ashx?t=", stock_sign)
      stock_rec <- htmltab(doc = fv_url, which = 9, header = 0)
      rec <- stock_rec[12,2]
      
      print("finviz")
      print("Analyst Recommendation")
      print("1-5 (1 Strong Buy | 5 Strong Sell)")
      print(rec)
      
   }
   
   stock_invest <- function(stock_sign){
      
      url <- paste0("https://stockinvest.us/technical-analysis/", stock_sign)
      url <- read_html(url)
      
      rec <- url %>%
         html_nodes("span") %>%
         html_text() %>%
         as.data.frame()
      
      names(rec)[1] <- "words"
      
      rec <- rec %>% filter(grepl("candidate", words))
      
      rec <-   as.data.frame(rec)
      rec <- gsub("candidate.*", "", rec$words)
      print("Stock Invest")
      print(rec)
      
   }
   
   market_watch <- function(stock_sign) {
      
      url <- paste0("https://www.marketwatch.com/investing/stock/", stock_sign, "/analystestimates")
      url <- read_html(url)
      
      stock_data <- url %>%
         html_nodes("td") %>%
         html_text() %>%
         str_squish() %>%
         as.data.frame()
      
      stock_data <- stock_data[-which(stock_data$. == ""), ]
      stock_data <- as.data.frame(stock_data)
      
      rec <- stock_data[29,1]
      rec <- sub(" .*", "", rec)
      rec <- as.character(rec)
      rec <- ifelse(is.na(rec), 'No Data', rec)
      
      print("Market Watch")
      print(rec)
      
   }
   
   markets_insider <- function(stock_sign){
      
      url <- paste0("https://markets.businessinsider.com/analyst/", stock_sign, "/all")
      url <- read_html(url)
      
      rec <- url %>%
         html_nodes("span") %>%
         html_text() %>%
         as.data.frame()
      
      names(rec)[1] <- "list"
      
      rec <- rec[!(rec$list == ""), ]
      rec <- as.data.frame(rec)
      rec <- rec[!(rec$rec == "Futures"), ]
      rec <- sub(" .*", "", rec)
      rec <- rec[2]
      rec <- ifelse(rec == "×", "No Data", rec)
      print("Markets Insider")
      print("1 = Buy | 5 = Sell")
      print(rec)
      
   }
   
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
      print(opinion)
      
   }
   
   market_beat <- function(stock_sign){
      url <- paste0("https://www.marketbeat.com/stocks/NYSE/", stock_sign, "/price-target/")
      url <- read_html(url)
      
      rec <- url %>%
         html_nodes("td") %>%
         html_text() %>%
         as.data.frame()
      
      rec <- rec[2,1]
      rec <- sub(" .*", "", rec)
      rec <- ifelse(rec == "Read", "Not NYSE", rec)
      print("Market Beat")
      print(rec)
   }
   
   wallet_investor <- function(stock_sign){
      
      url <- paste0("https://walletinvestor.com/stock-forecast/", stock_sign, "-stock-prediction")
      url <- read_html(url)
      
      words <- url %>%
         html_nodes("strong") %>%
         html_text() %>%
         as.data.frame()
      
      stock_name <- words[3,1]
      recommendation <- words[4,1]
      
      recommendation <- paste0(stock_name, "is a ", recommendation, " long term (1 year) investment.")
      print("Wallet Investor")
      print(recommendation)
      
   }
   
   yahoo_finance <- function(stock_sign){
      
      yahoo_url <- paste0("https://finance.yahoo.com/quote/", stock_sign, "?p=", stock_sign)
      url <- read_html(yahoo_url)
      words <- url %>%
         html_nodes(".IbBox") %>%
         html_text() %>%
         as.data.frame()
      
      est_return <- words[20,1]
      est_return <- as.character(est_return)
      
      # removing left side
      est_return <- gsub(".*XX", "\\1", est_return)
      # removing right side
      est_return <- gsub("Premium.*", "\\1", est_return)
      
      est_return <- ifelse(grepl(est_return, "-", fixed = FALSE) == TRUE, 
                           gsub("((\\d*))%"," i \\1%", est_return),
                           gsub("((\\d*))-"," i -\\1", est_return))
      
      # isolating fair value from % return
      value_ <-  sub(" i.*", "", est_return)    
      perc_return <-  sub(".*i ", "", est_return)    
      estimates <- paste0(value_, ' (', perc_return, ')')
      
      print("Yahoo Finance")
      print(estimates)
      
   }
   
   cnn_money <- function(stock_sign) {
      
      cnn_url <- paste0("https://money.cnn.com/quote/forecast/forecast.html?symb=", stock_sign)
      url <- read_html(cnn_url)
      
      words1 <- url %>%
         html_nodes("p") %>%
         html_text() %>%
         as.data.frame()
      
      rec <- words1[3,1] 
      rec <- gsub("Move.*","\\1",rec)
      
      print("CNN Money")
      print(rec)
      
   }
   
   stock_analysis <- function(stock_sign){
      
      print(stock_sign)
      financhill(stock_sign)
      #zacks_stock_price(stock_sign) # blocked by wifi
      #finviz_stock_price(stock_sign)
      #stock_invest(stock_sign)
      #markets_insider(stock_sign) #find way to show analyst recommendation range
      market_beat(stock_sign)
      #bar_chart(stock_sign)
      yahoo_finance(stock_sign)
      wallet_investor(stock_sign)
      market_watch(stock_sign)
      cnn_money(stock_sign)
      
   }
   
   stock_analysis(stock_sign)
   print(" ")
   
}


stock_recs("ehth")


#for ( i in stock_list ){
   
#   tryCatch(
#      stock_recs(i), error = function(e){})
   
#}

# trefis website for price forecasts
# terminology explanation
