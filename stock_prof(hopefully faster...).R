#TESTING POSSIBLY FASTER FUNCTION####

# 1 month stock volatility
rm(list = ls())

setwd("~/Stock Average Price Projection")

#removing yahoo finance missing data
library(tidyverse)
library(htmltab)
library(rvest)
library(xml2) 

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
stocks <- as.data.frame(stocks)
stocks$stocks <- gsub("\\^.*","",stocks$stocks)
stocks <- unique(stocks$stocks)

rm("amex")
rm("nasdaq")
rm("nyse")

# adding spaces to isolate stock symbols
stocks <- paste0(" ", stocks, sep="")
stocks <- paste(stocks, ' ', sep='')

# below is the code to remove specific stocks
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' ACCP ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' ACLL ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' AHI ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' AHL ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' AKO ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' ALIN ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' ALIT ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' ALP ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' ALTU ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' AUUD ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' AUUDW ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' BBRX ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' BCAC ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' BCACU ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' BCTX ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' BF ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' BML ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' BMY~ ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' BRK ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' BWL ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CELG~ ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CMPX ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CPTI ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CRD ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CTA ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CTEST ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' DRMT ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' DTLA ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' ECC ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' ECOM ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' EP ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' ETI ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' ETP ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' FLRZ ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' FLWR ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' FMAX ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' GHVI ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' GIX~ ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' GRP ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' HKIT ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' HLM ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' HYGO ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' IGLE ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' INAB ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' JAQC ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' JAQCU ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' JCIC ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' JCICU ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' JMPNZ ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' JW ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' KINZ ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' KWAC ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' LEGO ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' LEGOU ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' LGF ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' MER ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' MH ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' MOG ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' NGLS ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' NLSP ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' NLSPW ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' NMK ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' NTEST ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' NXMD ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' NYMT ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' OAK ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' OIBR ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' OTG ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' PCIM ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' PE ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' PRE ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' PRIF ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' PRK ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' PXMD ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' PXMDW ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' RDS ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' RMPL ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' SCCI ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' SCE ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' SPEL ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' SRVA ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' TVAC ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' VERT ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' VETS ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' VIV$ ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' VTEC ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' WNFM ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' ZTEST ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks$stocks %>% str_squish()
stocks <- as.data.frame(stocks)


month_profit_range <- function(stock_sign){
   
   # current price
   yahoo_url <- paste0("https://finance.yahoo.com/quote/", stock_sign, "?p=", stock_sign)
   
   current <- read_html(yahoo_url)
   
   current <- current %>%
      html_nodes("span") %>%
      html_text() %>%
      as.data.frame()
   
   current1 <- current[24,1]
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
   
   # profit percent difference
   profit_diff_perc = pot_prof_perc - pot_loss_perc
   profit_diff_perc = round(profit_diff_perc, digits=2)
   
   results = paste0(stock_sign," | Current Price: ", current, " | Profit ($) = $", profit_diff_num, " | Profit (%) = ", profit_diff_perc, "%")
   
   ifelse(profit_diff_num > 1 & profit_diff_perc > 90, print(results), "")
}

for ( i in stocks ){
   
   tryCatch(
      
      month_profit_range(i), error = function(e){})
   
}
