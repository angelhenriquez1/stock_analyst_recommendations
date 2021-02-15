# GOOGLE STOCK DATA

rm(list = ls())

library(tidyverse)
library(htmltab)
library(rvest)
library(xml2) 

stock_sign = "BABA"
# current price
google_url <- paste0("https://www.google.com/search?q=", stock_sign, "+stock")

current <- read_html(google_url)

current1 <- current %>%
   html_nodes("div") %>%
   html_text() %>%
   as.data.frame()

current1 <- unique(current1$.)
current1 <- as.data.frame(current1)




