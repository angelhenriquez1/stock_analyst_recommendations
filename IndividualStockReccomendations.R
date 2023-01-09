# Stock Recommendations Webscraping

# Clear data, if needed
#rm(list = ls())

#Package libraries####
library(tidyverse)
library(rvest)
library(XML)
library(xml2)
library(stringr)
library(tryCatchLog)
library(lubridate)

#Stock Recommendation Function####
stock_recs <- function(stock_sign) {

   # Financhill
   financhill <- function(stock_sign) {
   financhill_rec <- tryCatch({financhill_rec <- read_html(paste0("https://financhill.com/stock-forecast/", stock_sign, "-stock-prediction")) %>%
   html_nodes("div.stock-score-text") %>%
   html_text() %>%
   str_trim() %>%
   .[1]},
   error = function(e){NA})

   print("Financhill Recommendation")
   financhill_rec <- ifelse(financhill_rec=="Now", print("No Data"), print(financhill_rec))
   }

   # BarChart
   bar_chart <- function(stock_sign) {
   
      bar_chart <- tryCatch({bar_chart <- read_html(paste0("https://www.barchart.com/stocks/quotes/", stock_sign)) %>%
      html_nodes(".rating") %>%
      html_text() %>%
      str_trim()},
      error = function(e){NA})
  
   print("BarChart Recommendation")
   print(bar_chart)
   }
   
   # Zack's Rank
   zacks_rank <- function(stock_sign) {
      
      zacks_rank <- tryCatch({zacks_rank <- read_html(paste0("https://www.zacks.com/stock/research/", stock_sign, "/stock-style-scores")) %>%
         html_nodes("p.rank_view") %>%
         html_text() %>%
         str_trim() %>%
         .[1]},
         error = function(e){NA})
      
      zacks_rank <- tryCatch({zacks_rank <- gsub(".*-","",zacks_rank)},
                             error = function(e){NA})
      
      zacks_rank <- tryCatch({zacks_rank <- str_trim(gsub("of.*","",zacks_rank))},
                             error = function(e){NA})
      
      print("Zacks Recommendation")
      print(zacks_rank)
      
   }
   
   # MarketWatch
   marketwatch <- function(stock_sign) {
      
      marketwatch <- tryCatch({marketwatch <- read_html(paste0("https://www.marketwatch.com/investing/stock/", stock_sign, "/analystestimates")) %>%
         html_nodes("td.table__cell.w25") %>%
         html_text()%>%
         str_trim() %>%
         .[1]},
         error = function(e){NA})
      
      print("Market Watch Recommendation")
      print(marketwatch)
   }
   
   # CNN
   cnn <- function(stock_sign) {
      
   CNN <- tryCatch({CNN <- read_html(paste0("http://markets.money.cnn.com/research/quote/forecasts.asp?symb=", stock_sign)) %>%
      html_nodes("strong.wsod_rating") %>%
      html_text()},
      error = function(e){NA})
   
   print("CNN Recommendation")
   print(CNN)
   
   }
   
   # TipRanks
   tipRanks <- function(stock_sign) {
      
      tipRanks1 <- read_html(paste0("https://www.tipranks.com/stocks/", stock_sign, "/stock-analysis")) %>%
         html_nodes(".flexrbb.displayflex") %>%
         html_text() %>%
         .[33] %>%
         gsub(".*Consensus","",.)
      
      tipRanks2 <- read_html(paste0("https://www.tipranks.com/stocks/", stock_sign, "/stock-analysis")) %>%
         html_nodes(".flexrbb.displayflex") %>%
         html_text() %>%
         .[32] %>%
         gsub(".*Consensus","",.)
      
      tipRanks = paste0(tipRanks1, "~", tipRanks2)
      
      print("TipRanks Recommendation")
      print(tipRanks)
      
   }
   
   markets_insider <- function(stock_sign) {
   
   # Markets Insider
   markets_insider <- tryCatch({markets_insider <- read_html(paste0("https://markets.businessinsider.com/stocks/", stock_sign, "-stock")) %>%
      html_nodes("span.analysis-chart__value") %>%
      html_text() %>%
      str_trim()},
      error = function(e){NA})
   
   markets_insider <- tryCatch({markets_insider <- paste0(round((as.numeric(markets_insider[2]))/(as.numeric(markets_insider[1])),2)*100,"% Buy Rating by ", as.numeric(markets_insider[1]), " analysts.")},
                               error = function(e){NA})

   print("Markets Insider")
   print(markets_insider)

   }
   
   wallet_investor <- function(stock_sign) {
      
      # Wallet Investor
      wallet_investor <- tryCatch({ wallet_investor <- read_html(paste0("https://walletinvestor.com/stock-forecast/", stock_sign, "-stock-prediction")) %>%
         html_nodes("div.investment span") %>%
         html_text() %>%
         .[3] %>% 
         gsub("\\t", "", .) %>%
         gsub("\\r", "", .) %>%
         gsub("\\n", "", .) %>%
         gsub("   ", "", ., fixed = TRUE)},
         error = function(e){NA})
      
      print("Wallet Investor")
      print(wallet_investor)
   
   }
   
   yahoo_finance <- function(stock_sign) {
      
      # Yahoo Finance
      yahoo_finance1 <- tryCatch({ 
         yahoo_finance1 <- read_html(paste0("https://finance.yahoo.com/quote/", stock_sign, "?p=", stock_sign)) %>%
            html_nodes("div .Fw\\(b\\)") %>%
            html_text() %>%
            .[33]
      },
      error = function(e){NA})
   
      yahoo_finance2 <- tryCatch({ 
         yahoo_finance2 <- read_html(paste0("https://finance.yahoo.com/quote/", stock_sign, "?p=", stock_sign)) %>%
            html_nodes("div .Fw\\(b\\)") %>%
            html_text() %>%
            .[35]
      },
      error = function(e){NA})
      
      yahoo_finance <- tryCatch({
         yahoo_finance <- ifelse(yahoo_finance1 == "Near Fair Value" | yahoo_finance1 == "Undervalued" | yahoo_finance1 == "Overvalued",
                                 yahoo_finance1, yahoo_finance2)
         },
         error = function(e){NA})
      
      print("Yahoo Finance")
      print(yahoo_finance)  
   
   }
  
    barrons <- function(stock_sign) {
      # Barron's Stock Rating
      barrons <- tryCatch({barrons <- read_html(paste0("https://www.barrons.com/market-data/stocks/", stock_sign, "/research-ratings?mod=quotes")) %>%
         html_nodes(".sc-e320928e-2.hKvOmJ") %>% 
         html_text()}, error = function(e){NA})
      
      barrons_buy <- tryCatch(as.numeric(barrons[8]))
      barrons_overweight <- tryCatch(as.numeric(barrons[12]))
      barrons_hold <- tryCatch(as.numeric(barrons[16]))
      barrons_underweight <- tryCatch(as.numeric(barrons[20]))
      barrons_sell <- tryCatch(as.numeric(barrons[24]))
      
      average_buy_rating = tryCatch(round((barrons_buy+barrons_overweight)/(barrons_buy+barrons_overweight+barrons_hold+barrons_underweight+barrons_sell)*100,2))
      average_hold_rating = tryCatch(round((barrons_hold)/(barrons_buy+barrons_overweight+barrons_hold+barrons_underweight+barrons_sell)*100,2))
      average_sell_rating = tryCatch(round((barrons_underweight+barrons_sell)/(barrons_buy+barrons_overweight+barrons_hold+barrons_underweight+barrons_sell)*100,2))
      
      print("Barron's")
      print(paste0(average_buy_rating,"% Buy Rating"))
      # print(paste0(average_hold_rating,"% Hold"))
      # print(paste0(average_sell_rating,"% Sell"))
      
    }
    
    finviz <- function(stock_sign) {
       
       finviz <- tryCatch({finviz <- read_html(paste0("https://finviz.com/quote.ashx?t=", stock_sign,"&p=d")) %>%
          html_table() %>% 
          .[9] %>%
          .[[1]] %>%
          .[12,2] %>%
          as.numeric()},
          error = function(e){"No data"})
       
       print("Finviz Rating")
       finviz_rating <- ifelse(0 <= finviz & finviz <= 1, print("Strong Buy"), 
                               ifelse(1 < finviz & finviz <= 2, print("Buy"), 
                                      ifelse(2 < finviz & finviz <= 3, print("Hold"), 
                                             ifelse(3 < finviz & finviz <= 4, print("Sell"),
                                                    ifelse(4 < finviz & finviz <= 5, print("Strong Sell"), print(finviz))))))
       
    }
   
    marketbeat <- function(stock_sign) {
       
       # Marketbeat
       marketbeat <- tryCatch({marketbeat <- read_html(paste0("https://www.marketbeat.com/stocks/NYSE/", stock_sign, "/")) %>%
          html_nodes(".key-stat") %>% 
          html_text() %>%
          .[1]},
          error = function(e){NA})
       
       print("Marketbeat")
       print(marketbeat)  
       
    }
    
   # Running individual functions
   financhill(stock_sign)
   bar_chart(stock_sign)
   zacks_rank(stock_sign)
   marketwatch(stock_sign)
   cnn(stock_sign)
   tipRanks(stock_sign)
   markets_insider(stock_sign)
   wallet_investor(stock_sign)
   barrons(stock_sign)
   finviz(stock_sign)
   marketbeat(stock_sign)
   yahoo_finance(stock_sign)
   }

stock_recs("GOOG")
