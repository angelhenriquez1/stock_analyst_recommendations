# Put/ Call ratio

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
   
   ifelse(pc_ratio > 1, print(pc_ratio_buy), "")
   ifelse(pc_ratio < 0.75, print(pc_ratio_sell), "")
   ifelse(pc_ratio <= 1 & pc_ratio >= 0.75, print(pc_ratio_neutral), "")

}

for (i in stock_names){
   tryCatch(
      
      pc_ratio(i), error = function(e){})

}
