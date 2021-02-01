# pulling out data of top rated stocks from thestreet website
# https://www.thestreet.com/stock-market-news/10579592/top-rated-stocks/top-rated-stocks.html


# https://www.thestreet.com/topic/25821/top-rated-equity-airlines.html

library(tidyverse) #general
library(xml2) #read_html()
library(rvest) #html_nodes()
library(htmltab) #htmltab()
# current price
thestreet_url <- "https://www.thestreet.com/topic/25821/top-rated-equity-airlines.html"

current <- read_html(thestreet_url)

rating_link <- paste0('.', ' rating')

a <- current %>%
   html_nodes(".yui-gen20") %>%
   html_text() %>%
   as.data.frame()


# look at how I pulled nbc datain r code


url2 <- "https://www.thestreet.com/topic/25821/top-rated-equity-airlines.html"
thepage = readLines(url2)
mypattern = '<div ([^<]*)</div>'

pe = grep(mypattern, thepage, value = TRUE)
view(pe)

pe <- pe[2]
pe <- pe %>% str_squish()
pe <- gsub("<p class=\"data lastcolumn\"",'',pe)
pe <- gsub(">",'', pe)
pe <- gsub("</p",'', pe)
pe <- paste0("PE = ", pe)







