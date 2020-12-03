# Identifying Yahoo Finance Patterns

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

#Finding Patterns####
pattern_finding <- function(stock_sign){
yahoo_url <- paste0("https://finance.yahoo.com/quote/", stock_sign, "/")

url <- read_html(yahoo_url)

pattern <- url %>%
   html_nodes("p") %>%
   html_text() %>%
   as.data.frame()

pattern <- pattern[2,1]

# removing right side
pattern <- gsub("()).*", "\\1", pattern)
info <- paste0(stock_sign, " has a ", pattern, " pattern.")
ifelse(pattern == "Continuation Wedge (Bullish)", print(info), "")

}

for( i in data() ){
   
   tryCatch(
      
      pattern_finding(i), error = function(e){})
   
}

