library(rvest)
library(data.table)
library(stringi)
library(ggplot2)

monthUSD = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

url = "https://coinmarketcap.com/currencies/bitcoin/historical-data/?start=20130428&end=20171224"

bitcoin_hist = url %>% 
  read_html %>% 
  html_node("table.table") %>% 
  html_table %>% 
  as.data.table

names(bitcoin_hist)[names(bitcoin_hist) == "Market Cap"] = "MarketCap"

bitcoin_hist[,Volume := as.numeric(stri_replace_all_fixed(Volume,",",""))]

bitcoin_hist[,MarketCap := as.numeric(stri_replace_all_fixed(MarketCap,",",""))]

bitcoin_hist[,Date := stri_replace_all_fixed(Date,",","")]
bitcoin_hist[, c("month", "day", "year") := tstrsplit(Date, " ", fixed=TRUE)]
bitcoin_hist[,month:=match(month,monthUSD)]
bitcoin_hist[,Date := as.Date(paste0(year,"-",month,"-",day),format="%Y-%m-%d")]
bitcoin_hist[, c("month", "day", "year") := NULL]

# write.table(bitcoin_hist,"~/Downloads/telegramBitCoin/bitcoin_hist.txt",row.names = F, quote = F, sep = "\t")

long_bitcoin_hist = melt.data.table(bitcoin_hist,id.vars=c("Date","Low","High"),variable.name = "Measure",value.name = "Value")

ggplot(long_bitcoin_hist[!Measure%in%c("Volume","MarketCap")],aes(x=Date,y=Value,colour=Measure,ymin=Low,ymax=High)) + geom_ribbon(fill = 'grey80', colour = 'grey80') + geom_line() + scale_color_brewer(palette = "Set1") + scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") + theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.border = element_blank(), axis.line =  element_line(color="black")) + xlab("") + ylab("Price (USD)")


ggplot(long_bitcoin_hist[Date >= '2017-11-01' & !Measure%in%c("Volume","MarketCap")],aes(x=Date,y=Value,colour=Measure,ymin=Low,ymax=High)) + geom_ribbon(fill = 'grey80', colour = 'grey80') + geom_line() + geom_point() + scale_color_brewer(palette = "Set1") + scale_x_date(date_breaks = "1 day", date_labels =  "%b %Y") + theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.border = element_blank(), axis.line =  element_line(color="black")) + xlab("") + ylab("Price (USD)")
ggsave("~/Downloads/out_dez.png")
