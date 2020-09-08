

library("shiny")
library(quantmod)
library(lubridate)
library(BatchGetSymbols)
library(jsonlite)
library(tidyverse)
library(reactable)
library(plotly)
library(tidyquant)
library(plogr)
library(mongolite)

api_key = "485038cc0823a442c82b0a27d7adfbe1"

#tickers from mongo and formatted
tickers_recent = mongo(db = "local",
                       collection = "download_stocks",
                       url = "mongodb://thiq:WaLLet68@34.225.247.125:27017/")
tickers = tickers_recent$find("{}")$upload_stocks

#recent volatility from mongo and formatted

recent_volatility_collection = mongo(db = "local",
                                     collection = "recent_volatility",
                                     url = "mongodb://thiq:WaLLet68@34.225.247.125:27017/")
progress = 0
recent_volatility = list()
for (tkr in tickers) {
  stock = recent_volatility_collection$find(paste0('{"', tkr, '":{', '"$exists":true}}'))
  recent_volatility[[tkr]] = stock[[tkr]][[1]] %>%
    tibble()
  
  progress = progress + 1
  print(paste0(as.character(round((progress / length(tickers) * 100), digits = 2
  )), "% done"))
}

#trend finder from mongo and formatted
all_trends_collection = mongo(db = "local",
                              collection = "all_trends",
                              url = "mongodb://thiq:WaLLet68@34.225.247.125:27017/")

progress = 0
trend_sp_russell_output = list()
for (tkr in tickers) {
  stock = all_trends_collection$find(paste0('{"', tkr, '":{', '"$exists":true}}'))
  if (nrow(stock) > 0) {
    trend_sp_russell_output[[tkr]] = stock[[tkr]][[1]] %>%
      tibble() %>%
      mutate(ref_date = as_date(ref_date))
    
    progress = progress + 1
    print(paste0(as.character(round((progress / length(tickers) * 100), digits = 2
    )), "% done"))
    
  } else {
    progress = progress + 1
    print(paste0(as.character(round((progress / length(tickers) * 100), digits = 2
    )), "% done"))
  }
}

json_file = paste0(
  "https://financialmodelingprep.com/api/v3/key-metrics/",
  "AAPL",
  "?period=quarter&apikey=",
  api_key
)
base_fin = fromJSON(json_file) %>%
  tibble() %>%
  mutate(date = as_date(.$date), year = year(date))

default_fin = c(
  "symbol",
  "date",
  "year",
  "peRatio",
  "debtToEquity",
  "pbRatio",
  "freeCashFlowPerShare",
  "roe",
  "currentRatio",
  "grahamNumber"
)

red_to_wt = function(x)
  rgb(colorRamp(c("#FFFFFF", "#f87274"))(x), maxColorValue = 255)

wt_to_green = function(x)
  rgb(colorRamp(c("#FFFFFF", "#63be7b"))(x), maxColorValue = 255)

function(input,
         output, session) {
  #undervalued company finder ----------------------------------------------------------
  
  date_start_date = reactive({
    date = as.Date(input$date_range[1], origin = "1970-01-01")
    
  })
  
  # date_start_date = date_start_date()
  
  date_end_date = reactive({
    date = as.Date(input$date_range[2], origin = "1970-01-01")
    
  })
  
  # date_end_date = date_end_date()
  
  updateSelectInput(session,
                    "companies",
                    choices = tickers)
  
  desired_stock_change_data = reactive({
    #Pulling the weekly stock change data. Filters from recent_volatilit.rds file
    
    filtered_date = lapply(
      recent_volatility,
      filter,
      week >= week(date_start_date()),
      week <= week(date_end_date())
    )
    
    filtered_date[lapply(lapply(
      filtered_date,
      filter,
      abs(price_change) > input$change_threshold
    ),
    nrow) > 0]
    
  })
  
  observeEvent(input$update_companies,
               {
                 updateSelectInput(
                   session,
                   "companies",
                   choices = names(desired_stock_change_data()),
                   selected = head(names(desired_stock_change_data()), 3)
                 )
                 
                 updateSelectInput(session,
                                   "fin_values",
                                   choices = names(base_fin),
                                   selected = default_fin)
                 
               })
  
  #Company 1 -----------------------------------------
  
  output$comp1_price_changes = renderReactable({
    shiny::validate(need(
      input$companies != "",
      "Please add companies or update the company list"
    ))
    
    comp = input$companies[1]
    
    data_frame_stock_change = as.data.frame(desired_stock_change_data()[[comp]])
    
    reactable(
      data = data_frame_stock_change,
      groupBy = "month",
      columns = list(
        month = colDef(
          name = "Month",
          align = "left",
          width = 70
        ),
        year = colDef(
          name = "Year",
          align = "left",
          width = 70
        ),
        week = colDef(
          name = "Week",
          align = "left",
          width = 70
        ),
        avg_zscore = colDef(show = FALSE),
        price_change = colDef(
          name = "Price Change",
          align = "left",
          aggregate = "sum",
          format = colFormat(percent = TRUE),
          style = function(value) {
            if (value >= input$change_threshold) {
              background = wt_to_green(1)
              color = ""
            } else if (value > 0) {
              background = wt_to_green(value * 1 / input$change_threshold)
              color = ""
            } else if (abs(value) >= input$change_threshold) {
              background = red_to_wt(1)
              color = ""
            } else {
              background = red_to_wt(-value * 1 / input$change_threshold)
              color = ""
            }
            list(background = background,
                 color = color ,
                 fontWeight = "bold")
          }
        ),
        future_change = colDef(
          name = "Future Price Change",
          align = "left",
          format = colFormat(percent = TRUE)
        ),
        avg_price = colDef(
          name = "Average Price",
          align = "left",
          aggregate = "mean",
          format = colFormat(currency = "USD")
        ),
        past_price = colDef(
          name = "Average Past Price",
          align = "left",
          aggregate = "mean",
          format = colFormat(currency = "USD")
        )
      ),
      sortable = TRUE,
      defaultPageSize = 4,
      defaultSorted = c("month", "week"),
      defaultSortOrder = "desc",
      highlight = TRUE,
      bordered = TRUE,
      compact = TRUE
    )
    
  })
  
  
  
  output$comp1_fundamentals = renderReactable({
    shiny::validate(need(input$companies != "", ""))
    
    comp = input$companies[1]
    
    json_file = paste0(
      "https://financialmodelingprep.com/api/v3/key-metrics/",
      comp,
      "?period=quarter&apikey=485038cc0823a442c82b0a27d7adfbe1"
    )
    base_data = fromJSON(json_file) %>%
      tibble() %>%
      mutate(date = as_date(.$date), year = year(date))
    
    num_data = base_data[lapply(base_data, class) == "numeric"] %>%
      round(digits = 2)
    
    comp_fin_data = base_data[!lapply(base_data, class) == "numeric"] %>%
      select(symbol, date, everything()) %>%
      bind_cols(num_data)
    
    reactable(
      data = comp_fin_data %>% select(input$fin_values),
      groupBy = "year",
      sortable = TRUE,
      defaultPageSize = 4,
      defaultSorted = c("year", "date"),
      defaultSortOrder = "desc",
      highlight = TRUE,
      bordered = TRUE
    )
    
  })
  
  #Company 2 -----------------------------
  
  output$comp2_price_changes = renderReactable({
    shiny::validate(need(input$companies != "", ""))
    
    comp = input$companies[2]
    
    data_frame_stock_change = as.data.frame(desired_stock_change_data()[[comp]])
    
    reactable(
      data = data_frame_stock_change,
      groupBy = "month",
      columns = list(
        month = colDef(
          name = "Month",
          align = "left",
          width = 70
        ),
        year = colDef(
          name = "Year",
          align = "left",
          width = 70
        ),
        week = colDef(
          name = "Week",
          align = "left",
          width = 70
        ),
        avg_zscore = colDef(show = FALSE),
        price_change = colDef(
          name = "Price Change",
          align = "left",
          aggregate = "sum",
          format = colFormat(percent = TRUE),
          style = function(value) {
            if (value >= input$change_threshold) {
              background = wt_to_green(1)
              color = ""
            } else if (value > 0) {
              background = wt_to_green(value * 1 / input$change_threshold)
              color = ""
            } else if (abs(value) >= input$change_threshold) {
              background = red_to_wt(1)
              color = ""
            } else {
              background = red_to_wt(-value * 1 / input$change_threshold)
              color = ""
            }
            list(background = background,
                 color = color ,
                 fontWeight = "bold")
          }
        ),
        future_change = colDef(
          name = "Future Price Change",
          align = "left",
          format = colFormat(percent = TRUE)
        ),
        avg_price = colDef(
          name = "Average Price",
          align = "left",
          aggregate = "mean",
          format = colFormat(currency = "USD")
        ),
        past_price = colDef(
          name = "Average Past Price",
          align = "left",
          aggregate = "mean",
          format = colFormat(currency = "USD")
        )
      ),
      sortable = TRUE,
      defaultPageSize = 4,
      defaultSorted = c("month", "week"),
      defaultSortOrder = "desc",
      highlight = TRUE,
      bordered = TRUE,
      compact = TRUE
    )
    
  })
  
  
  
  output$comp2_fundamentals = renderReactable({
    shiny::validate(need(input$companies != "", ""))
    
    comp = input$companies[2]
    
    json_file = paste0(
      "https://financialmodelingprep.com/api/v3/key-metrics/",
      comp,
      "?period=quarter&apikey=485038cc0823a442c82b0a27d7adfbe1"
    )
    base_data = fromJSON(json_file) %>%
      tibble() %>%
      mutate(date = as_date(.$date), year = year(date))
    
    num_data = base_data[lapply(base_data, class) == "numeric"] %>%
      round(digits = 2)
    
    comp_fin_data = base_data[!lapply(base_data, class) == "numeric"] %>%
      select(symbol, date, everything()) %>%
      bind_cols(num_data)
    
    reactable(
      data = comp_fin_data %>% select(input$fin_values),
      groupBy = "year",
      sortable = TRUE,
      defaultPageSize = 4,
      defaultSorted = c("year", "date"),
      defaultSortOrder = "desc",
      highlight = TRUE,
      bordered = TRUE
    )
    
  })
  
  #Company 3 ----------------------------------------
  
  output$comp3_price_changes = renderReactable({
    shiny::validate(need(input$companies != "", ""))
    
    comp = input$companies[3]
    
    data_frame_stock_change = as.data.frame(desired_stock_change_data()[[comp]])
    
    reactable(
      data = data_frame_stock_change,
      groupBy = "month",
      columns = list(
        month = colDef(
          name = "Month",
          align = "left",
          width = 70
        ),
        year = colDef(
          name = "Year",
          align = "left",
          width = 70
        ),
        week = colDef(
          name = "Week",
          align = "left",
          width = 70
        ),
        avg_zscore = colDef(show = FALSE),
        price_change = colDef(
          name = "Price Change",
          align = "left",
          aggregate = "sum",
          format = colFormat(percent = TRUE),
          style = function(value) {
            if (value >= input$change_threshold) {
              background = wt_to_green(1)
              color = ""
            } else if (value > 0) {
              background = wt_to_green(value * 1 / input$change_threshold)
              color = ""
            } else if (abs(value) >= input$change_threshold) {
              background = red_to_wt(1)
              color = ""
            } else {
              background = red_to_wt(-value * 1 / input$change_threshold)
              color = ""
            }
            list(background = background,
                 color = color ,
                 fontWeight = "bold")
          }
        ),
        future_change = colDef(
          name = "Future Price Change",
          align = "left",
          format = colFormat(percent = TRUE)
        ),
        avg_price = colDef(
          name = "Average Price",
          align = "left",
          aggregate = "mean",
          format = colFormat(currency = "USD")
        ),
        past_price = colDef(
          name = "Average Past Price",
          align = "left",
          aggregate = "mean",
          format = colFormat(currency = "USD")
        )
      ),
      sortable = TRUE,
      defaultPageSize = 4,
      defaultSorted = c("month", "week"),
      defaultSortOrder = "desc",
      highlight = TRUE,
      bordered = TRUE,
      compact = TRUE
    )
    
  })
  
  
  
  output$comp3_fundamentals = renderReactable({
    shiny::validate(need(input$companies != "", ""))
    
    comp = input$companies[3]
    
    json_file = paste0(
      "https://financialmodelingprep.com/api/v3/key-metrics/",
      comp,
      "?period=quarter&apikey=485038cc0823a442c82b0a27d7adfbe1"
    )
    base_data = fromJSON(json_file) %>%
      tibble() %>%
      mutate(date = as_date(.$date), year = year(date))
    
    num_data = base_data[lapply(base_data, class) == "numeric"] %>%
      round(digits = 2)
    
    comp_fin_data = base_data[!lapply(base_data, class) == "numeric"] %>%
      select(symbol, date, everything()) %>%
      bind_cols(num_data)
    
    reactable(
      data = comp_fin_data %>% select(input$fin_values),
      groupBy = "year",
      sortable = TRUE,
      defaultPageSize = 4,
      defaultSorted = c("year", "date"),
      defaultSortOrder = "desc",
      highlight = TRUE,
      bordered = TRUE
    )
    
  })
  
  
  # desired_stock_change_data = desired_stock_change_data()
  
  #trend_Finder data ------------------------------------------------
  
  trend_start_date = reactive({
    as.Date(input$trend_date_range[1], origin = "1970-01-01")
  })
  
  
  trend_end_date = reactive({
    as.Date(input$trend_date_range[2], origin = "1970-01-01")
  })
  
  
  spec_trend_data = reactive({
    usable_data = lapply(
      trend_sp_russell_output,
      filter,
      between(ref_date, trend_start_date(), trend_end_date())
    )
    
    if (input$trend_direction == "Bullish") {
      usable_data[lapply(
        lapply(
          usable_data,
          filter,
          week_trend[1] >= input$trend_thresh &
            week_trend[2] >= input$trend_thresh &
            week_trend[3] >= input$trend_thresh &
            week_trend[4] >= input$trend_thresh &
            week_trend[5] >= input$trend_thresh &
            week_trend[6] &
            avg_price[1] > input$min_price
        ),
        nrow
      ) > 0]
      
    } else {
      usable_data[lapply(
        lapply(
          usable_data,
          filter,-week_trend[1] >= input$trend_thresh &
            -week_trend[2] >= input$trend_thresh &
            -week_trend[3] >= input$trend_thresh &
            -week_trend[4] >= input$trend_thresh &
            -week_trend[5] >= input$trend_thresh &
            -week_trend[6] &
            avg_price[1] > input$min_price
        ),
        nrow
      ) > 0]
      
    }
    
  })
  
  observeEvent(input$update_company,
               {
                 updateSelectInput(
                   session,
                   "company",
                   choices = names(spec_trend_data()),
                   selected = head(names(spec_trend_data()), 1)
                 )
                 
               })
  
  chart_start_date = reactive({
    as.Date(input$chart_date_range[1], origin = "1970-01-01")
  })
  # date_start_date = date_start_date()
  
  chart_end_date = reactive({
    as.Date(input$chart_date_range[2], origin = "1970-01-01")
  })
  
  output$chart_series = renderPlot({
    shiny::validate(need(input$company != "", ""))
    
    comp = input$company[1]
    
    json_file = paste0(
      "https://financialmodelingprep.com/api/v3/historical-price-full/",
      comp,
      "?apikey=",
      api_key
    )
    
    chart_date_data = fromJSON(json_file)$historical %>%
      tibble() %>%
      mutate(date = as.Date(date)) %>%
      select(date, everything()) %>%
      dplyr::filter(between(date, chart_start_date(), chart_end_date()))
    
    json_data = fromJSON(json_file)$historical %>%
      tibble() %>%
      mutate(date = as.Date(date)) %>%
      select(date, everything()) %>%
      arrange(date) %>%
      ggplot(aes(x = date, y = close)) +
      geom_candlestick(aes(
        open = open,
        high = high,
        low = low,
        close = close
      )) +
      geom_ma(
        ma_fun = SMA,
        n = input$SMA1,
        linetype = 12,
        size = 1,
        show.legend = TRUE
      ) +
      geom_ma(
        ma_fun = SMA,
        n = input$SMA2,
        color = "red",
        size = 1,
        show.legend = TRUE
      ) +
      geom_ma(
        ma_fun = EMA,
        n = input$EMA1,
        color = "green" ,
        linetype = 12,
        size = 1,
        show.legend = TRUE
      ) +
      coord_x_date(
        xlim = c(chart_start_date(), chart_end_date()),
        ylim = c(min(chart_date_data$low), max(chart_date_data$high))
      ) +
      theme_tq()
    
    json_data
    
  })
  
  output$interactive_plot = renderPlotly({
    shiny::validate(need(input$company != "", ""))
    
    comp = input$company[1]
    
    json_file = paste0(
      "https://financialmodelingprep.com/api/v3/historical-price-full/",
      comp,
      "?apikey=",
      api_key
    )
    
    plot = fromJSON(json_file)$historical %>%
      tibble() %>%
      mutate(date = as.Date(date)) %>%
      select(date, everything()) %>%
      arrange(date) %>%
      ggplot(aes(x = date, y = close)) +
      geom_line(aes(x = date, y = close)) +
      geom_line(aes(
        y = rollmean(close, input$SMA1, na.pad = TRUE),
        colour = paste0(input$SMA1, "-day SMA")
      ), na.rm = TRUE) +
      geom_line(aes(
        y = rollmean(close, input$SMA2, na.pad = TRUE),
        colour = paste0(input$SMA2, "-day SMA")
      ), na.rm = TRUE) +
      geom_line(aes(
        y = rollmean(close, input$EMA1, na.pad = TRUE),
        colour = paste0(input$EMA1, "-day EMA")
      ), na.rm = TRUE) +
      theme(
        legend.position = c(0.5, 0.95),
        legend.justification = c("left", "top")
      )
    
    ggplotly(plot)
    
  })
  
  output$trend_data = renderReactable({
    shiny::validate(need(input$company != "", ""))
    
    large_day_gain = .05
    large_week_gain = .1
    large_2_week_gain = .10
    large_4_week_gain = .15
    large_6_month_gain = .4
    
    comp = trend_sp_russell_output[[input$company[1]]]
    
    print(comp)
    
    reactable(
      comp,
      groupBy = c("year", "month"),
      sortable = TRUE,
      defaultSorted = c("year", "month", "week", "ref_date"),
      defaultSortOrder = "desc",
      columns = list(
        month = colDef(
          name = "Month",
          align = "left",
          width = 70
        ),
        year = colDef(
          name = "Year",
          align = "left",
          width = 70
        ),
        week = colDef(
          name = "Week",
          align = "left",
          width = 70
        ),
        ref_date = colDef(
          name = "Date",
          align = "left",
          width = 70
        ),
        avg_price = colDef(
          name = "Average Price",
          align = "left",
          aggregate = "mean",
          format = list(
            cell = colFormat(currency = "USD"),
            aggregated = colFormat(currency = "USD")
          )
        ),
        past_price = colDef(
          name = "Previous Price",
          align = "left",
          aggregate = "mean",
          format = list(
            cell = colFormat(currency = "USD"),
            aggregated = colFormat(currency = "USD")
          )
        ),
        volume = colDef(show = FALSE),
        day_trend = colDef(
          name = "Daily Change",
          aggregate = "mean",
          align = "left",
          format = list(
            cell = colFormat(percent = TRUE, digits = 2),
            aggregated = colFormat(percent = TRUE, digits = 2)
          ),
          style = function(value) {
            if (value >= large_day_gain) {
              background = wt_to_green(1)
              color = ""
            } else if (value > 0) {
              background = wt_to_green(value / large_day_gain)
              color = ""
            } else if (abs(value) >= large_day_gain) {
              background = red_to_wt(1)
              color = ""
            } else {
              background = red_to_wt(-value / large_day_gain)
              color = ""
            }
            list(background = background,
                 color = color ,
                 fontWeight = "bold")
          }
        ),
        week_trend = colDef(
          name = "Weekly Change",
          aggregate = "mean",
          align = "left",
          format = list(
            cell = colFormat(percent = TRUE, digits = 2),
            aggregated = colFormat(percent = TRUE, digits = 2)
          ),
          style = function(value) {
            if (value >= large_week_gain) {
              background = wt_to_green(1)
              color = ""
            } else if (value > 0) {
              background = wt_to_green(value / large_week_gain)
              color = ""
            } else if (abs(value) >= large_week_gain) {
              background = red_to_wt(1)
              color = ""
            } else {
              background = red_to_wt(-value / large_week_gain)
              color = ""
            }
            list(background = background,
                 color = color ,
                 fontWeight = "bold")
          }
        ),
        running_2_week_trend = colDef(
          name = "2 Week Change",
          aggregate = "mean",
          align = "left",
          format = list(
            cell = colFormat(percent = TRUE, digits = 2),
            aggregated = colFormat(percent = TRUE, digits = 2)
          ),
          style = function(value) {
            if (value >= large_2_week_gain) {
              background = wt_to_green(1)
              color = ""
            } else if (value > 0) {
              background = wt_to_green(value / large_2_week_gain)
              color = ""
            } else if (abs(value) >= large_2_week_gain) {
              background = red_to_wt(1)
              color = ""
            } else {
              background = red_to_wt(-value / large_2_week_gain)
              color = ""
            }
            list(background = background,
                 color = color ,
                 fontWeight = "bold")
          }
        ),
        running_4_week_trend = colDef(
          name = "Monthly Change",
          aggregate = "mean",
          align = "left",
          format = list(
            cell = colFormat(percent = TRUE, digits = 2),
            aggregated = colFormat(percent = TRUE, digits = 2)
          ),
          style = function(value) {
            if (value >= large_4_week_gain) {
              background = wt_to_green(1)
              color = ""
            } else if (value > 0) {
              background = wt_to_green(value / large_4_week_gain)
              color = ""
            } else if (abs(value) >= large_4_week_gain) {
              background = red_to_wt(1)
              color = ""
            } else {
              background = red_to_wt(-value / large_4_week_gain)
              color = ""
            }
            list(background = background,
                 color = color ,
                 fontWeight = "bold")
          }
        ),
        running_6_month_trend = colDef(
          name = "6 Month Change",
          aggregate = "mean",
          align = "left",
          format = list(
            cell = colFormat(percent = TRUE, digits = 2),
            aggregated = colFormat(percent = TRUE, digits = 2)
          ),
          style = function(value) {
            if (value >= large_6_month_gain) {
              background = wt_to_green(1)
              color = ""
            } else if (value > 0) {
              background = wt_to_green(value / large_6_month_gain)
              color = ""
            } else if (abs(value) >= large_6_month_gain) {
              background = red_to_wt(1)
              color = ""
            } else {
              background = red_to_wt(-value / large_6_month_gain)
              color = ""
            }
            list(background = background,
                 color = color ,
                 fontWeight = "bold")
          }
        )
      )
    )
  })
  
  
  #stock analyzer page ------------------------------------
  
  stock_data = reactive({
    comp = input$comp[1]
    sp_russell_daily_collection = mongo(db = "local",
                                        collection = "sp_russell_daily",
                                        url = "mongodb://thiq:WaLLet68@34.225.247.125:27017/")
    daily_prices = sp_russell_daily_collection$find(paste0('{"ticker":"', comp, '"}')) %>%
      tibble()
    
  })
  
  trend_data = reactive({
    stock_data() %>%
      mutate(
        year = year(ref_date),
        month = month(ref_date),
        week = week(ref_date)
      ) %>%
      group_by(year, month, week, ref_date) %>%
      summarise(avg_price = round(mean(price_close), 2),
                volume = mean(volume)) %>%
      ungroup() %>%
      mutate(
        past_price = round(lag(avg_price, 1), 2),
        day_trend = round((avg_price - lag(avg_price, 1)) / lag(avg_price, 1), 2),
        week_trend = round((avg_price - lag(avg_price, 5)) / lag(avg_price, 5), 2),
        running_2_week_trend = round((avg_price - lag(avg_price, 10)) /
                                       lag(avg_price, 10), 2),
        change_2_week_trend = round((running_2_week_trend - lag(running_2_week_trend)) / lag(running_2_week_trend),
                                    2
        ),
        running_4_week_trend = round((avg_price - lag(avg_price, 20)) /
                                       lag(avg_price, 20), 2),
        change_4_week_trend = round((running_4_week_trend - lag(running_4_week_trend)) / lag(running_4_week_trend),
                                    2
        ),
        running_6_month_trend =  round((avg_price - lag(avg_price, 120)) /
                                         lag(avg_price, 120), 2)
      ) %>%
      arrange(desc(ref_date))
    
  })
  
  observeEvent(input$update_single_comp,
               {
                 output$single_chart = renderPlot({
                   shiny::validate(need(input$comp[1] != "", ""))
                   
                   chart_start_date = input$single_chart_date_range[1]
                   chart_end_date = input$single_chart_date_range[2]
                   
                   comp = input$comp[1]
                   
                   json_file = paste0(
                     "https://financialmodelingprep.com/api/v3/historical-price-full/",
                     comp,
                     "?apikey=",
                     api_key
                   )
                   
                   chart_date_data = fromJSON(json_file)$historical %>%
                     tibble() %>%
                     mutate(date = as.Date(date)) %>%
                     select(date, everything()) %>%
                     dplyr::filter(between(date, chart_start_date, chart_end_date))
                   
                   json_data = fromJSON(json_file)$historical %>%
                     tibble() %>%
                     mutate(date = as.Date(date)) %>%
                     select(date, everything()) %>%
                     arrange(date) %>%
                     ggplot(aes(x = date, y = close)) +
                     geom_candlestick(aes(
                       open = open,
                       high = high,
                       low = low,
                       close = close
                     )) +
                     geom_ma(
                       ma_fun = SMA,
                       n = input$single_SMA1,
                       linetype = 12,
                       size = 1,
                       show.legend = TRUE
                     ) +
                     geom_ma(
                       ma_fun = SMA,
                       n = input$single_SMA2,
                       color = "red",
                       size = 1,
                       show.legend = TRUE
                     ) +
                     geom_ma(
                       ma_fun = EMA,
                       n = input$single_EMA1,
                       color = "green" ,
                       linetype = 12,
                       size = 1,
                       show.legend = TRUE
                     ) +
                     coord_x_date(
                       xlim = c(chart_start_date(), chart_end_date()),
                       ylim = c(min(chart_date_data$low), max(chart_date_data$high))
                     ) +
                     theme_tq()
                   
                   json_data
                   
                 })
                 
                 output$single_interactive_plot = renderPlotly({
                   shiny::validate(need(input$comp[1] != "", ""))
                   
                   comp = input$comp[1]
                   
                   json_file = paste0(
                     "https://financialmodelingprep.com/api/v3/historical-price-full/",
                     comp,
                     "?apikey=",
                     api_key
                   )
                   
                   plot = fromJSON(json_file)$historical %>%
                     tibble() %>%
                     mutate(date = as.Date(date)) %>%
                     select(date, everything()) %>%
                     arrange(date) %>%
                     ggplot(aes(x = date, y = close)) +
                     geom_line(aes(x = date, y = close)) +
                     geom_line(aes(
                       y = rollmean(close, input$single_SMA1, na.pad = TRUE),
                       colour = paste0(input$single_SMA1, "-day SMA")
                     ), na.rm = TRUE) +
                     geom_line(aes(
                       y = rollmean(close, input$single_SMA2, na.pad = TRUE),
                       colour = paste0(input$single_SMA2, "-day SMA")
                     ), na.rm = TRUE) +
                     geom_line(aes(
                       y = rollmean(close, input$single_EMA1, na.pad = TRUE),
                       colour = paste0(input$single_EMA1, "-day SMA")
                     ), na.rm = TRUE) +
                     theme(
                       legend.position = c(0.5, 0.95),
                       legend.justification = c("left", "top")
                     )
                   
                   ggplotly(plot)
                   
                 })
                 
                 output$single_trend_data = renderReactable({
                   shiny::validate(need(input$comp[1] != "", ""))
                   
                   large_day_gain = .05
                   large_week_gain = .1
                   large_2_week_gain = .10
                   large_4_week_gain = .15
                   large_6_month_gain = .4
                   
                   comp = na.omit(trend_data())
                   
                   reactable(
                     comp,
                     groupBy = c("year", "month"),
                     sortable = TRUE,
                     defaultSorted = c("year", "month", "week", "ref_date"),
                     defaultSortOrder = "desc",
                     columns = list(
                       month = colDef(
                         name = "Month",
                         align = "left",
                         width = 70
                       ),
                       year = colDef(
                         name = "Year",
                         align = "left",
                         width = 70
                       ),
                       week = colDef(
                         name = "Week",
                         align = "left",
                         width = 70
                       ),
                       ref_date = colDef(
                         name = "Date",
                         align = "left",
                         width = 70
                       ),
                       avg_price = colDef(
                         name = "Average Price",
                         align = "left",
                         aggregate = "mean",
                         format = list(
                           cell = colFormat(currency = "USD"),
                           aggregated = colFormat(currency = "USD")
                         )
                       ),
                       past_price = colDef(
                         name = "Previous Price",
                         align = "left",
                         aggregate = "mean",
                         format = list(
                           cell = colFormat(currency = "USD"),
                           aggregated = colFormat(currency = "USD")
                         )
                       ),
                       volume = colDef(show = FALSE),
                       day_trend = colDef(
                         name = "Daily Change",
                         aggregate = "mean",
                         align = "left",
                         format = list(
                           cell = colFormat(percent = TRUE, digits = 2),
                           aggregated = colFormat(percent = TRUE, digits = 2)
                         ),
                         style = function(value) {
                           if (value >= large_day_gain) {
                             background = wt_to_green(1)
                             color = ""
                           } else if (value > 0) {
                             background = wt_to_green(value / large_day_gain)
                             color = ""
                           } else if (abs(value) >= large_day_gain) {
                             background = red_to_wt(1)
                             color = ""
                           } else {
                             background = red_to_wt(-value / large_day_gain)
                             color = ""
                           }
                           list(background = background,
                                color = color ,
                                fontWeight = "bold")
                         }
                       ),
                       week_trend = colDef(
                         name = "Weekly Change",
                         aggregate = "mean",
                         align = "left",
                         format = list(
                           cell = colFormat(percent = TRUE, digits = 2),
                           aggregated = colFormat(percent = TRUE, digits = 2)
                         ),
                         style = function(value) {
                           if (value >= large_week_gain) {
                             background = wt_to_green(1)
                             color = ""
                           } else if (value > 0) {
                             background = wt_to_green(value / large_week_gain)
                             color = ""
                           } else if (abs(value) >= large_week_gain) {
                             background = red_to_wt(1)
                             color = ""
                           } else {
                             background = red_to_wt(-value / large_week_gain)
                             color = ""
                           }
                           list(background = background,
                                color = color ,
                                fontWeight = "bold")
                         }
                       ),
                       running_2_week_trend = colDef(
                         name = "2 Week Change",
                         aggregate = "mean",
                         align = "left",
                         format = list(
                           cell = colFormat(percent = TRUE, digits = 2),
                           aggregated = colFormat(percent = TRUE, digits = 2)
                         ),
                         style = function(value) {
                           if (value >= large_2_week_gain) {
                             background = wt_to_green(1)
                             color = ""
                           } else if (value > 0) {
                             background = wt_to_green(value / large_2_week_gain)
                             color = ""
                           } else if (abs(value) >= large_2_week_gain) {
                             background = red_to_wt(1)
                             color = ""
                           } else {
                             background = red_to_wt(-value / large_2_week_gain)
                             color = ""
                           }
                           list(background = background,
                                color = color ,
                                fontWeight = "bold")
                         }
                       ),
                       running_4_week_trend = colDef(
                         name = "Monthly Change",
                         aggregate = "mean",
                         align = "left",
                         format = list(
                           cell = colFormat(percent = TRUE, digits = 2),
                           aggregated = colFormat(percent = TRUE, digits = 2)
                         ),
                         style = function(value) {
                           if (value >= large_4_week_gain) {
                             background = wt_to_green(1)
                             color = ""
                           } else if (value > 0) {
                             background = wt_to_green(value / large_4_week_gain)
                             color = ""
                           } else if (abs(value) >= large_4_week_gain) {
                             background = red_to_wt(1)
                             color = ""
                           } else {
                             background = red_to_wt(-value / large_4_week_gain)
                             color = ""
                           }
                           list(background = background,
                                color = color ,
                                fontWeight = "bold")
                         }
                       ),
                       running_6_month_trend = colDef(
                         name = "6 Month Change",
                         aggregate = "mean",
                         align = "left",
                         format = list(
                           cell = colFormat(percent = TRUE, digits = 2),
                           aggregated = colFormat(percent = TRUE, digits = 2)
                         ),
                         style = function(value) {
                           if (value >= large_6_month_gain) {
                             background = wt_to_green(1)
                             color = ""
                           } else if (value > 0) {
                             background = wt_to_green(value / large_6_month_gain)
                             color = ""
                           } else if (abs(value) >= large_6_month_gain) {
                             background = red_to_wt(1)
                             color = ""
                           } else {
                             background = red_to_wt(-value / large_6_month_gain)
                             color = ""
                           }
                           list(background = background,
                                color = color ,
                                fontWeight = "bold")
                         }
                       )
                     )
                   )
                 })
                 
                 updateSelectInput(
                   session,
                   "single_fin_values",
                   choices = names(base_fin),
                   selected = default_fin
                 )
                 
                 output$single_fundamentals = renderReactable({
                   shiny::validate(need(input$comp[1] != "", ""))
                   
                   comp = input$comp[1]
                   
                   json_file = paste0(
                     "https://financialmodelingprep.com/api/v3/key-metrics/",
                     comp,
                     "?period=quarter&apikey=485038cc0823a442c82b0a27d7adfbe1"
                   )
                   base_data = fromJSON(json_file) %>%
                     tibble() %>%
                     mutate(date = as_date(.$date), year = year(date))
                   
                   num_data = base_data[lapply(base_data, class) == "numeric"] %>%
                     round(digits = 2)
                   
                   comp_fin_data = base_data[!lapply(base_data, class) == "numeric"] %>%
                     select(symbol, date, everything()) %>%
                     bind_cols(num_data)
                   
                   reactable(
                     data = comp_fin_data %>% select(input$single_fin_values),
                     groupBy = "year",
                     sortable = TRUE,
                     defaultPageSize = 4,
                     defaultSorted = c("year", "date"),
                     defaultSortOrder = "desc",
                     highlight = TRUE,
                     bordered = TRUE
                   )
                   
                 })
                 
               })
}