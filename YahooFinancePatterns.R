# IDENTIFYING YAHOO FINANCE PATTERNS
rm(list = ls())
# list of stock symbols

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
stocks <- stocks[!grepl(' ACAM ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' ACAMW ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' ACCP ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' ACLL ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' ADOCU ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' AGBAU ', stocks$stocks),]
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
stocks <- stocks[!grepl(' ARA ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' ATEST ', stocks$stocks),]
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
stocks <- stocks[!grepl(' BKT ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' BML ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' BMY~ ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' BREZU ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' BRK ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' BRPAU ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' BSMO ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' BSMR ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' BTT ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' BWL ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CAS ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CBO ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CBX ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CELG~ ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CHFW ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CMPX ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CNTX ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CNXCV ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CPTI ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CRD ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CTA ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CTEST ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' DDI ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' DMYI ', stocks$stocks),]
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
stocks <- stocks[!grepl(' EVGBC ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' EVLMC ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' EVSTC ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' FLRZ ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' FLWR ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' FMAX ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' FRX ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' GBLK ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' GGO ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' GHVI ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' GIX~ ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' GMVD ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' GNPK ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' GRP ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' GV ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' GWACU ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' HCAR ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' HCCHU ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' HFBL ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' HKIT ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' HLM ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' HYGO ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' IBO ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' IGLE ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' IIAC ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' INAB ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' JAQC ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' JAQCU ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' JBK ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' JCIC ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' JCICU ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' JMPNZ ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' JPT ', stocks$stocks),]
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
stocks <- stocks[!grepl(' LN ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' LNFA ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' LSACW ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' MBNKP ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' MNCLU ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' MVNR ', stocks$stocks),]
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
stocks <- stocks[!grepl(' OCA ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' OIBR ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' OTG ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' PCIM ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' PE ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' PIPP ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' PRE ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' PRIF ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' PRK ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' PRPLW ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' PSM ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' PTACU ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' PXMD ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' PXMDW ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' PXSAP ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' RDS ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' RELV ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' RMPL ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' RTPZ ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' SCCI ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' SCE ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' SFBC ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' SOHON ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' SOHOO ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' SPEL ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' SPRQ ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' SRVA ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' SVT ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' TINV ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' TOTAR ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' TOTAU ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' TVAC ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' TZACU ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' VERT ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' VETS ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' VGM ', stocks$stocks),]
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

