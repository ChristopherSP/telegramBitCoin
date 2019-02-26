baseURL = "http://www.xe.com/currencytables/?from=BRL&date="

CurrencyToBRL = function(date){
  url = paste0(baseURL,date)
  currencyToBRL = url %>% 
    read_html %>% 
    html_node("table#historicalRateTbl.tablesorter.historicalRateTable-table") %>% 
    html_table %>% 
    mutate(date = date) %>% 
    as.data.table
  
  currencyToBRL
}

SafeCurrencyToBRL = safely(CurrencyToBRL)

historicalCurrencyToBRL = lapply(bitcoin_hist$Date, SafeCurrencyToBRL)

historicalCurrencyToBRLDT = rbindlist(lapply(historicalCurrencyToBRL, function(x) x$result))
names(historicalCurrencyToBRLDT) = c("Code","Name","UnitsBRL","BRLUnit","Date")

# write.table(historicalCurrencyToBRLDT,"~/Downloads/telegramBitCoin/historicalCurrencyToBRL.txt",row.names = F,quote = F, sep = '\t')
