library(tidyquant)

url_btc = "https://min-api.cryptocompare.com/data/histoday?fsym=BTC&tsym=BRL&allData=true"
url_eth = "https://min-api.cryptocompare.com/data/histoday?fsym=ETH&tsym=BRL&allData=true"

btc = fromJSON(
  rawToChar(
    GET(url_btc)$content
  )
)

eth = fromJSON(
  rawToChar(
    GET(url_eth)$content
  )
)

dt_btc = as.data.table(btc$Data)
dt_btc[, diff := (open - close)/open]
dt_btc[, date := as.Date(as.POSIXct(time, origin = "1970-01-01", tz = "GMT"), format="%Y-%m-%d")]

dt_eth = as.data.table(eth$Data)
dt_eth[, diff := (open - close)/open]
dt_eth[, date := as.Date(as.POSIXct(time, origin = "1970-01-01", tz = "GMT"), format="%Y-%m-%d")]

ggplot(dt_btc[date >= today() - 6*30], aes(x = date, y = close, open = open, high = high, low = low, close = close)) +
  geom_candlestick() +
  geom_bbands(ma_fun = EMA, sd = 2, n = 30, color_ma = "grey30", color_bands = "grey70") +
  geom_smooth(method = "lm", se = F, colour = "black", linetype = "dashed", size = 0.7)+
  ggtitle("BTC", subtitle = "Last 6 months") +
  ylab("Closing Price (BRL)") +
  xlab("") +
  scale_x_date(date_breaks = '7 days', date_labels = "%d/%m/%y") +
  theme(
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(colour = "grey80"),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

dt_btc[, coin := "BTC"]
dt_eth[, coin := "ETH"]

dt_coins = rbind(dt_btc, dt_eth)

ggplot(dt_coins[date >= today() - 1*30], aes(x = date, y = close, open = open, high = high, low = low, close = close)) +
  geom_candlestick() +
  geom_bbands(ma_fun = EMA, sd = 2, n = 7, color_ma = "grey30", color_bands = "grey70") +
  geom_smooth(method = "lm", se = F, colour = "black", linetype = "dashed", size = 0.7)+
  ggtitle("Cryptocurrencies", subtitle = "Last Months") +
  ylab("Closing Price (BRL)") +
  xlab("") +
  facet_wrap(~coin, scales = "free_y", ncol = 1) +
  scale_x_date(date_breaks = '1 day', date_labels = "%d/%m/%y") +
  theme(
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(colour = "grey80"),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
