# List of symbols with low volatility or low price
library(tidyverse)
library(textreadr)
library(pdftools)

# the code below is to remove the symbols, but use it with the spaces before and after
# symbols to remove
sytr <- function(stock_names){

   
   stock_names <- paste(" ", stock_names, sep="")
   stock_names <- paste(stock_names," ", sep="")
   
   stock_names <- stock_names[stock_names != " AAU "]
   stock_names <- stock_names[stock_names != " ABEV "]
   stock_names <- stock_names[stock_names != " CELG "]
   stock_names <- stock_names[stock_names != " CELG "]
   stock_names <- stock_names[stock_names != " CELG "]
   stock_names <- stock_names[stock_names != " CELG "]
   stock_names <- stock_names[stock_names != " CELG "]
   
   
}

#data1 <- as.data.frame(data2)
data1 <- sytr(data)