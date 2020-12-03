# Daily Put/ Call ratio and Volume

#Library####
library(tidyverse) #general
library(xml2) #read_html()
library(rvest) #html_nodes()
library(htmltab) #htmltab()

#data####
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
   
   names(stock_names)[1] <- "stock_symbols"
   
   stock_names$stock_symbols <- as.character(stock_names$stock_symbols)
   stock_names$stock_symbols <- gsub("\\..*","", stock_names$stock_symbols)
   stock_names <- unique(stock_names$stock_symbols)
   
}

stock_sign = "goog"
pc_ratio <- function(stock_sign){

   put_call_ratio <- paste0("https://marketchameleon.com/Overview/", stock_sign, "/OpenInterestTrends/")
   
   current <- read_html(put_call_ratio)
   
   current <- current %>%
      html_nodes("td") %>%
      html_text() %>%
      as.data.frame()
   
   current <- current[!apply(is.na(current) | current == "", 1, all),]
   current <- as.data.frame(current)
   current <- current[grep("[[:digit:]]", current$current), ]
   current <- as.data.frame(current)
   current <- current[!grepl("Day", current$current),]
   current <- as.data.frame(current)
   pc_ratio <- current[7,1]
   pc_ratio <- as.numeric(gsub("\\-.*","", pc_ratio))
   
   interest_level <- current[1,1]
   interest_level <- as.numeric(gsub(",", "", interest_level))
   
   pc_ratio_buy <- paste0(stock_sign, ": Buy | PCR = ", pc_ratio, " | Interest Level = ", interest_level)
   pc_ratio_sell <- paste0(stock_sign, ": Sell | PCR = ", pc_ratio, " | Interest Level = ", interest_level)
   pc_ratio_neutral <- paste0(stock_sign, ": Neutral | PCR = ", pc_ratio, " | Interest Level = ", interest_level)
   
   ifelse(pc_ratio > 1 & interest_level > 100000, print(pc_ratio_buy), "")
   #ifelse(pc_ratio < 0.75, print(pc_ratio_sell), "")
   #ifelse(pc_ratio <= 1 & pc_ratio >= 0.75 & interest_level > 100000, print(pc_ratio_neutral), "")
   
}


#pc_ratio("baba")

for (i in data()){
   tryCatch(
      
      pc_ratio(i), error = function(e){})

}
