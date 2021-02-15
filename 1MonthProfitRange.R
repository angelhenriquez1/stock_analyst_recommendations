# 1 MONTH STOCK PROFIT RANGES
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

# merging stocks
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

#Removing Stocks####
# adding spaces to isolate stock symbols
stocks <- paste0(" ", stocks, sep="")
stocks <- paste(stocks, ' ', sep='')

# below is the code to remove specific stocks

#A####
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' AACQW ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' AAIC ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' AAU ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' ABEO ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' ABEV ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' ACAMW ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' ACCP ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' ACEVW ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' ACLL ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' ACRX ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' ACST ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' ADIL ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' ADILW ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' ADMA ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' ADMP ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' ADOCU ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' ADVWW ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' ADXS ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' AEHR ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' AEMD ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' AENZ ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' AESE ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' AEY ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' AEZS ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' AGBAR ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' AGBAW ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' AGBAU ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' AGE ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' AGFS ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' AGRX ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' AHACW ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' AHC ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' AHI ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' AHL ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' AHT ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' AIHS ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' AIKI ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' AIM ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' AIRI ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' AIRTW ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' AKO ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' AKTX ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' AKU ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' ALACR ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' ALACW ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' ALIN ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' ALJJ ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' ALIT ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' ALNA ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' ALP ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' ALRN ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' ALSK ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' ALTU ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' AMHCW ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' AMPE ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' AMPY ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' AMRH ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' AMRHW ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' AMS ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' ANDAR ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' ANDAW ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' ANH ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' APOP ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' ARA ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' ARC ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' ARPO ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' ARTL ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' ARTLW ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' ARTW ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' ASLN ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' ASM ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' ASRT', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' AT ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' ATCXW ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' ATEST ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' ATHE ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' ATHX ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' ATIF ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' ATNFW ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' AUMN ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' AUUD ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' AUUDW ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' AVCO ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' AVCTW ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' AVGR ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' AXU ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' AZRX ', stocks$stocks),]
stocks <- as.data.frame(stocks)

#B####
stocks <- stocks[!grepl(' BBAR ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' BBGI ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' BBI ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' BBRX ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' BCAC ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' BCACU ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' BCDAW ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' BCTX ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' BDR ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' BEST ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' BF ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' BGI ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' BHAT ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' BIMI ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' BIOL ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' BKCC ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' BKEP ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' BKT ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' BML ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' BMY~ ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' BORR ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' BOSC ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' BOWXW ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' BOXL ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' BREZU ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' BRK ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' BRLIR ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' BRLIW ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' BROGW ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' BRPAR ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' BRPAU ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' BRQS ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' BSMO ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' BSMR ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' BTT ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' BTAQW ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' BTE ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' BTN ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' BWL ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' BXRX ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' BYFC ', stocks$stocks),]
stocks <- as.data.frame(stocks)

#C####
stocks <- stocks[!grepl(' CALA ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CANF ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CAPAW ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CAS ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CASI ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CBO ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CBX ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CCLP ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CCO ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CDEV ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CDTX ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CEI ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CELG~ ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CELP ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CEPU ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CETX ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CETXP ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CETXW ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CFIIW ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CFMS ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CGROW ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CHEK ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CHEKZ ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CHFW ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CHNR ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CHPMW ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CHS ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CIDM ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CIF ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CIG ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CIH ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CIK ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CJJD ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CLRB ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CLRBZ ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CLRO ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CLSN ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CMPX ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CNF ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CNTX ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CNXCV ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' COCP ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CPAH ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CPG ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CPHI ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CPIX ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CPTI ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CREX ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CREXW ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CRD ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CRHM ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CRSAW ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CRTDW ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CSCW ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CSLT ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CTA ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CTEK ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CTEST ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CTHR ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CTIB ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CTRM ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CTXR ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CTXRW ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CWBR ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CXDC ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CYAN ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' CYRN ', stocks$stocks),]
stocks <- as.data.frame(stocks)

#D####
stocks <- stocks[!grepl(' DARE ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' DBDRW ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' DDI ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' DFFN ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' DFPHW ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' DGLY ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' DHF ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' DHX ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' DHY ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' DLNG ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' DLPNW ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' DMYI ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' DNK ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' DNN ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' DOGZ ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' DRAD ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' DRIOW ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' DRMT ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' DRRX ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' DRTT ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' DS ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' DSKEW ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' DSWL ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' DSX ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' DTLA ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' DVD ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' DWSN ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' DXF ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' DXLG ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' DYNT ', stocks$stocks),]
stocks <- as.data.frame(stocks)

#E####
stocks <- stocks[!grepl(' EAST ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' ECC ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' ECOM ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' ECOR ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' EDN ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' EGY ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' EMAN ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' EMX ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' ENSV ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' ENTX ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' ENTXW ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' ENZ ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' EP ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' EQS ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' ERESW ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' ESGC ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' ESSCR ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' ESSCW ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' ETACW ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' ETI ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' ETP ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' ETTX ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' EVC ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' EVGBC ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' EVOL ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' EVLMC ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' EVSTC ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' EXN ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' EYES ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' EYESW ', stocks$stocks),]
stocks <- as.data.frame(stocks)

#F####
stocks <- stocks[!grepl(' FAMI ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' FCACU ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' FEDU ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' FENG ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' FGB ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' FI ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' FIIIW ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' FLMN ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' FLMNW ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' FLRZ ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' FLWR ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' FMAX ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' FPAY ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' FRBK ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' FREEW ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' FRX ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' FTIVW ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' FTK ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' FURY ', stocks$stocks),]
stocks <- as.data.frame(stocks)

#G####
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

#H####
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

#I####
stocks <- stocks[!grepl(' IBO ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' IGLE ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' IIAC ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' INAB ', stocks$stocks),]
stocks <- as.data.frame(stocks)

#J####
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

#K####
stocks <- stocks[!grepl(' KINZ ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' KWAC ', stocks$stocks),]
stocks <- as.data.frame(stocks)

#L####
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

#M####
stocks <- stocks[!grepl(' MBNKP ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' MDVL ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' MDVR ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' MNCLU ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' MVNR ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' MH ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' MOG ', stocks$stocks),]
stocks <- as.data.frame(stocks)

#N####
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

#O####
stocks <- stocks[!grepl(' OAK ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' OCA ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' OIBR ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' OTG ', stocks$stocks),]
stocks <- as.data.frame(stocks)

#P####
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

#Q####

#R####
stocks <- stocks[!grepl(' RDS ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' RELV ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' RMPL ', stocks$stocks),]
stocks <- as.data.frame(stocks)
stocks <- stocks[!grepl(' RTPZ ', stocks$stocks),]
stocks <- as.data.frame(stocks)

#S####
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

#T####
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

#U####

#V####
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

#W####
stocks <- stocks[!grepl(' WNFM ', stocks$stocks),]
stocks <- as.data.frame(stocks)

#X####

#Y####

#Z####
stocks <- stocks[!grepl(' ZTEST ', stocks$stocks),]


#1 Month Profit Range####
month_profit_range <- function(stock_sign){

   # current price
   yahoo_url <- paste0("https://finance.yahoo.com/quote/", stock_sign, "?p=", stock_sign)
   
   current <- read_html(yahoo_url)

   current <- current %>%
      html_nodes("span") %>%
      html_text() %>%
      as.data.frame()
   
   current <- as.data.frame(current[!apply(is.na(current) | current == "", 1, all),])
   current <- as.data.frame(current[grep("[[:digit:]]", current$current), ])
   current <- as.data.frame(current[!grepl("W", current$current),])
   current1 <- current[12,1]
   current1 <- gsub(",","",current1)
   current1 <- suppressWarnings(as.numeric(as.character(current1)))
   current2 <- current[16,1]
   current2 <- gsub(",","",current2)
   current2 <- suppressWarnings(as.numeric(as.character(current2)))
   current <- ifelse(is.na(current1) == TRUE, current2, current1)
   current <- as.data.frame(current)
   current <- as.numeric(as.character(current$current))
   
   # barchart data
   url = paste0("https://www.barchart.com/stocks/quotes/", stock_sign, "/overview")
   data <- suppressWarnings(htmltab(doc = url, which = 1, header = 0))
   
   # 1 month high
   high <- data[1,3]
   high <- gsub("\\-.*","", high)
   high <- str_replace_all(high, ",", "")
   high <- as.numeric(as.character(high))
   
   # 1 month low
   low <- data[1,2]
   low <- gsub("\\+.*","", low)
   low <- str_replace_all(low, ",", "")
   low <- suppressWarnings(as.numeric(as.character(low)))
   
   # 1 month high low difference
   dif = high - low
   
   # number gain
   pot_prof = high - current
   # percent gain
   pot_prof_perc = (pot_prof/dif)*100
   
   # number loss
   pot_loss = current - low
   # percent loss
   pot_loss_perc = (pot_loss/dif)*100
   
   # profit difference number
   profit_diff_num = pot_prof - pot_loss
   profit_diff_num = round(profit_diff_num, digits=2)

   # profit percent difference
   profit_diff_perc = pot_prof_perc - pot_loss_perc
   profit_diff_perc = round(profit_diff_perc, digits=2)
   
   results = paste0(stock_sign," | Current Price: ", current, " | Profit ($) = $", profit_diff_num, " | Profit (%) = ", profit_diff_perc, "%")
   # both greater than 80% and $10 profit potential

   ifelse(profit_diff_num > 1 & profit_diff_perc > 85, print(results), "")
   
}

for ( i in stocks$stocks ){
tryCatch( month_profit_range(i), error = function(e){print})
}

