# finding earnings whisper estimates

#Library####
library(tidyverse) #general
library(xml2) #read_html()
library(rvest) #html_nodes()
library(htmltab) #htmltab()

whisper <- function(stock_sign){
   
   url = paste0("https://www.earningswhispers.com/stocks/", stock_sign)

   url <- read_html(url)

   # estimated price
   info <- url %>%
      html_nodes("div.mainitem") %>%
      html_text() %>%
      as.data.frame()

   est <- info[1,1]

   est <- sub('.', '', est)

   est <- as.numeric(as.character(est))

   # earnings date
   date <- info[2,1]
   
   date <- sub('', '', date)
   
   # consensus EPS estimate
   consensus <- url %>%
      html_nodes("div#consensus") %>%
      html_text() %>%
      as.data.frame()

   consensus <- consensus[1,1]
   consensus <- str_squish(sub('Consensus:', '', consensus))
   consensus <- sub('.', '', consensus)
   consensus <- as.numeric(as.character(consensus))

# "estimated eps difference"
   profit_potential <- est - consensus
   num_pot <- paste0("$", profit_potential)

# "" as %
   percent_potential <- ((est - consensus)/consensus)*100
   percent_potential <- round(percent_potential, digits = 2)
   perc_pot <- paste0(percent_potential, "%")
   
   profitable <- paste0(stock_sign, " | ", num_pot, " | ", perc_pot, " | ", date)

   ifelse(profit_potential > 0.1, print(profitable), "")

}

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

# loop for all stock tickers in data
for (i in data()) {
   tryCatch(whisper(i),error = function(e){})
}
