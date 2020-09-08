library(quantmod)
library(lubridate)
library(BatchGetSymbols)
library(jsonlite)
library(tidyverse)
library(tidyquant)
library(reactable)
library(mongolite)


api_key = "485038cc0823a442c82b0a27d7adfbe1"

#tickers ---------------------------
# russell_tickers = read_csv("russell2000.csv") %>%
#   .[[1]]
# russell2000_stocks = as.list(getSymbols(head(russell_tickers,5)))
#
# SP = GetSP500Stocks()
#
# tkrs = c(SP[,1],na.omit(russell_tickers[1:2000]))
# tkrs = as.data.frame(tkrs)
#
tickers_col = mongo(db = "local", collection = "tickers_col", url = "mongodb://thiq:WaLLet68@34.225.247.125:27017/")
#
# tickers_col$insert(tkrs)

tickers = tickers_col$find("{}")$tkrs

#stock data ------------------------------
sp_russell_daily_collection = mongo(db = "local",
                                    collection = "sp_russell_daily",
                                    url = "mongodb://thiq:WaLLet68@34.225.247.125:27017/")

sp_russell_current = sp_russell_daily_collection$find('{"ticker":"AAPL", "ref_date":{"$gt":"2020-07-13"}}') #works, but getting it takes about 4 minutes

max_date = as.Date(max(sp_russell_current$ref_date))

sp_russell_daily = data.frame()

sp_russell_daily = BatchGetSymbols(
  tickers = tickers,
  first.date = max_date + 1,
  last.date = Sys.Date(),
  freq.data = "daily",
  thresh.bad.data = 0,
  do.cache = FALSE
) %>% #no cache because it is temporary data
  .$df.tickers

sp_russell_daily_collection = mongo(db = "local",
                                    collection = "sp_russell_daily",
                                    url = "mongodb://thiq:WaLLet68@34.225.247.125:27017/")

sp_russell_daily_collection$insert(sp_russell_daily)

sp_russell_daily = sp_russell_daily_collection$find("{}")

# sp_russell_daily = sp_russell_daily$df.tickers %>%
#   bind_rows(sp_russell_five_years$df.tickers) %>%
#   na.omit()

#recent volatility data --------------------
min_thresh = .01
max_month = c(1:month(Sys.Date()))
max_year = 2020
min_price = 0

progress = 0
default_sp_russell_output = list()
for (sym in tickers) {
  default_sp_russell_output[[sym]] = as_tibble(sp_russell_daily) %>%
    filter(sym == ticker) %>%
    mutate(
      zscore = round(abs(volume - mean(volume)) / sd(volume), 2),
      month = month(ref_date),
      year = year(ref_date),
      month = month(ref_date),
      week = week(ref_date)
    ) %>%
    group_by(year, month, week) %>%
    summarise(avg_zscore = round(mean(zscore), 2),
              avg_price = round(mean(price_close), 2)) %>%
    ungroup() %>%
    mutate(
      past_price = round(lag(avg_price, 1), 2),
      price_change = round((avg_price - lag(avg_price, 1)) / lag(avg_price, 1), 2),
      future_change = round((avg_price - lead(avg_price, 1)) / lead(avg_price, 1), 2)
    ) %>%
    filter(
      avg_price > min_price &
        abs(price_change) >= min_thresh &
        year == max_year &
        month %in% max_month
    ) %>% #filtering for march, april, may, june in 2020 with a change over 50%
    arrange(desc(abs(week)))
  progress = progress + 1
  print(paste0(as.character(round((progress / length(tickers) * 100), digits = 2
  )), "% done"))
  
}

recent_volatility = default_sp_russell_output[lapply(default_sp_russell_output, nrow) > 0]

recent_volatility_collection = mongo(db = "local",
                                     collection = "recent_volatility",
                                     url = "mongodb://thiq:WaLLet68@34.225.247.125:27017/")

#clear current
recent_volatility_collection$remove("{}")

#upload new
progress = 0
for (x in 1:length(recent_volatility)) {
  recent_volatility_collection$insert(recent_volatility[x])
  
  progress = progress + 1
  print(paste0(as.character(round((progress / length(recent_volatility) * 100), digits = 2
  )), "% done"))
}

#trend finder mongo upload
progress = 0
trend_sp_russell_output = list()
for (sym in tickers) {
  trend_sp_russell_output[[sym]] = as_tibble(sp_russell_daily) %>%
    filter(sym == ticker) %>%
    mutate(
      year = year(ref_date),
      month = month(ref_date),
      week = week(ref_date),
      ref_date = as.Date(ref_date)
    ) %>%
    group_by(year, month, week, ref_date) %>%
    summarise(avg_price = round(mean(price_close), 2), volume = mean(volume)) %>%
    ungroup() %>%
    mutate(
      past_price = round(lag(avg_price, 1), 2),
      day_trend = round((avg_price - lag(avg_price, 1)) / lag(avg_price, 1), 2),
      week_trend = round((avg_price - lag(avg_price, 5)) / lag(avg_price, 5), 2),
      running_2_week_trend = round((avg_price - lag(avg_price, 10)) /
                                     lag(avg_price, 10), 2),
      change_2_week_trend = round((
        running_2_week_trend - lag(running_2_week_trend)
      ) / lag(running_2_week_trend), 2),
      running_4_week_trend = round((avg_price - lag(avg_price, 20)) /
                                     lag(avg_price, 20), 2),
      change_4_week_trend = round((
        running_4_week_trend - lag(running_4_week_trend)
      ) / lag(running_4_week_trend), 2),
      running_6_month_trend =  round((avg_price - lag(avg_price, 120)) /
                                       lag(avg_price, 120), 2)
    ) %>%
    arrange(desc(ref_date))
  progress = progress + 1
  print(paste0(as.character(round((progress / length(tickers) * 100), digits = 2
  )), "% done"))
}

upload_data = lapply(trend_sp_russell_output,
                     filter,
                     between(ref_date, Sys.Date() - 180, Sys.Date()))

min_trend_thresh = .05

pos_trends = upload_data[lapply(
  lapply(
    upload_data,
    filter,
    week_trend[1] >= min_trend_thresh &
      week_trend[2] >= min_trend_thresh &
      week_trend[3] >= min_trend_thresh &
      week_trend[4] >= min_trend_thresh &
      week_trend[5] >= min_trend_thresh &
      week_trend[6] &
      avg_price[1] > 10
  ),
  nrow
) > 0]

neg_trends = upload_data[lapply(
  lapply(
    upload_data,
    filter,-week_trend[1] >= min_trend_thresh &
      -week_trend[2] >= min_trend_thresh &
      -week_trend[3] >= min_trend_thresh &
      -week_trend[4] >= min_trend_thresh &
      -week_trend[5] >= min_trend_thresh &
      -week_trend[6] &
      avg_price[1] > 10
  ),
  nrow
) > 0]


#recent trends rather than all of the above
all_trends = c(pos_trends, neg_trends)

all_trends_collection =  mongo(db = "local",
                               collection = "all_trends",
                               url = "mongodb://thiq:WaLLet68@34.225.247.125:27017/")

all_trends_collection$remove("{}")

progress = 0

for (x in 1:length(all_trends)) {
  all_trends_collection$insert(all_trends[x])
  
  progress = progress + 1
  print(paste0(as.character(round((progress / length(all_trends) * 100), digits = 2
  )), "% done"))
}

trend_finder_collection = mongo(db = "local",
                                collection = "trend_finder",
                                url = "mongodb://thiq:WaLLet68@34.225.247.125:27017/")

trend_finder_collection$remove("{}")

progress = 0
for (x in 1:length(trend_sp_russell_output)) {
  trend_finder_collection$insert(trend_sp_russell_output[x])
  
  progress = progress + 1
  print(paste0(as.character(round((progress / length(trend_sp_russell_output) * 100), digits = 2
  )), "% done"))
}

#collecting all possible tickers for recent and for all_trends
names(all_trends)

filtered_date = lapply(recent_volatility,
                       filter,
                       between(week, week(Sys.Date() - 60), week(Sys.Date())),
                       year == 2020)

min_recent_thresh = .15

upload_stocks = c(names(filtered_date[lapply(lapply(
  filtered_date,
  filter,
  abs(price_change) > min_recent_thresh & avg_price > 5
),
nrow) > 0]), names(all_trends)) %>%
  unique()

download_stocks_collection = mongo(db = "local",
                                   collection = "download_stocks",
                                   url = "mongodb://thiq:WaLLet68@34.225.247.125:27017/")

download_stocks_collection$remove("{}")

download_stocks_collection$insert(as.data.frame(upload_stocks))
