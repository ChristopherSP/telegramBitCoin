library(telegram)
library(jsonlite)
library(tidyverse)

bot <- TGBot$new(token = bot_token('CryptoBot'))
bot$getMe()

msgs <- bot$getUpdates()

bot$set_default_chat_id(msgs$message$chat$id[1])

bot$sendPhoto("~/Downloads/out_dez.png")

safe_fromJSON <- safely(fromJSON, as.numeric(NA)) 

nova_consulta_list <- safe_fromJSON("https://api.blinktrade.com/api/v1/BRL/ticker?crypto_currency=BTC") 

nova_consulta <- nova_consulta_list$result %>% 
  as.tibble %>%
  mutate(timestamp = lubridate::now())

nova_consulta
