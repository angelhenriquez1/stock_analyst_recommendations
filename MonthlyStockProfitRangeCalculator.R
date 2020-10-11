# 1 Month Stock Profit Range Calculator

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

#Monthly Profit Potential####
# All I need is 1 month stock price range data


