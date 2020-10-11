# 1 month stock volatility

#Library####

#library(tidyverse) #general
#library(xml2) #read_html()
#library(rvest) #html_nodes()
#library(htmltab) #htmltab()


#Stock Symbols####
# finding list of all stock symbols

#data <- function(){

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
   #view(data)
   data1 <- tibble(data)
   data1$data <- paste(" ", data1$data, sep="")
   data1$data <- paste(data1$data,' ', sep="")

#}

test_data <- data()

test_data <- toString(test_data)

test_data <- sapply(strsplit(test_data, '[, ]+'), function(x) toString(dQuote(x)))

newdata <- month_profit_range(test_data)

#1 Month Profit Range####
month_profit_range <- function(stock_sign){

   stock_sign <- as.character(stock_sign)
   
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
   current1 <- current[12,1]
   current1 <- gsub(",","",current1)
   current1 <- suppressWarnings(as.numeric(as.character(current1)))
   current2 <- current[16,1]
   current2 <- gsub(",","",current2)
   current2 <- suppressWarnings(as.numeric(as.character(current2)))
   current <- ifelse(is.na(current1) == TRUE, current2, current1)
   current <- as.data.frame(current)
   current <- as.numeric(as.character(current$current))
   
   # 1 month high and low
   url = paste0("https://www.barchart.com/stocks/quotes/", stock_sign, "/overview")
   
   #barchart data
   data <- suppressWarnings(htmltab(doc = url, which = 1, header = 0))
   
   #1 month high
   high <- data[1,3]
   high <- gsub("\\-.*","", high)
   high <- suppressWarnings(as.numeric(as.character(high)))
   
   #1 month low
   low <- data[1,2]
   low <- gsub("\\+.*","", low)
   low <- suppressWarnings(as.numeric(as.character(low)))
   
   #1 month high low difference
   dif = high - low
   
   #number gain
   pot_prof = high - current
   #percent gain
   pot_prof_perc = (pot_prof/dif)*100
   
   #number loss
   pot_loss = current - low
   #percent loss
   pot_loss_perc = (pot_loss/dif)*100
   
   #profit difference number
   profit_diff_num = pot_prof - pot_loss
   profit_diff_num = round(profit_diff_num, digits=2)
   profit_diff_num1 = paste0(stock_sign," has $", profit_diff_num, " profit potential.")
   
   #profit percent difference
   profit_diff_perc = pot_prof_perc - pot_loss_perc
   profit_diff_perc = round(profit_diff_perc, digits=2)
   profit_diff_perc1 = paste0(stock_sign, " has ", profit_diff_perc, "% profit potential.")
   
   #conditionals
   
   ifelse(profit_diff_perc > 70, print(profit_diff_perc1), "")
   ifelse(profit_diff_num > 20, print(profit_diff_num1), "")
   
}

month_profit_range("usm")

for ( i in test_data ){
   
   tryCatch(
      month_profit_range(i), error = function(e){})
   
}
