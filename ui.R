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


navbarPage(
  "Volatile Stock Info",
  tabPanel(
    title = "Undervalued Company Finder",
    fluidPage(
      fluidRow(
        column(
          dateRangeInput(
            "date_range",
            "Select date range",
            start = Sys.Date() - 30,
            end = Sys.Date() + 1,
            min = "2020-01-01",
            max = Sys.Date() + 2
          ),
          width = 3
        ),
        column(
          sliderInput(
            "change_threshold",
            "Select Minimum Percent Change",
            min = 0.05,
            max  = 1.00,
            value = 0.3
          ),
          width = 3
        ),
        column(
          selectInput(
            "companies",
            "Select Companies",
            multiple = TRUE,
            choices = NULL
          ),
          width = 3
        ),
        column(
          selectInput(
            "fin_values",
            "Select Financial Values",
            multiple = TRUE,
            choices = NULL,
            selected = default_fin
          ),
          width = 3
        ),
        actionButton("update_companies", label = "Update Company List", width = "50%")
      ),
      br(),
      h2("First company financial information"),
      fluidRow(
        column(
          reactableOutput("comp1_price_changes", height = "400px"),
          width = 5
        ),
        column(
          reactableOutput("comp1_fundamentals", height = "400px"),
          width = 7
        )
      ),
      br(),
      h2("Second company financial information"),
      fluidRow(
        column(
          reactableOutput("comp2_price_changes", height = "400px"),
          width = 5
        ),
        column(
          reactableOutput("comp2_fundamentals", height = "400px"),
          width = 7
        )
      ),
      br(),
      h2("Third company financial information"),
      fluidRow(
        column(
          reactableOutput("comp3_price_changes", height = "400px"),
          width = 5
        ),
        column(
          reactableOutput("comp3_fundamentals", height = "400px"),
          width = 7
        )
      ),
      br()
    )
  ),
  tabPanel(title = "Trend Finder",
           fluidPage(
             sidebarLayout(
               titlePanel("Find recent trends in SP500 and Russell 2000!"),
               sidebarPanel(
                 dateRangeInput(
                   "trend_date_range",
                   "Select Date Range",
                   start = Sys.Date() - 30,
                   end = Sys.Date() + 1,
                   min = "2020-01-01",
                   max = Sys.Date() + 1
                 ),
                 sliderInput(
                   "trend_thresh",
                   "Choose a change % minimum by week!",
                   min = 0.01,
                   max = 0.20,
                   value = 0.09
                 ),
                 numericInput("min_price",
                              "Choose a minimum price",
                              value = 30),
                 radioButtons(
                   "trend_direction",
                   "Bullish or Bearish?",
                   choices = c("Bullish", "Bearish"),
                   selected = "Bullish"
                 ),
                 actionButton("update_company",
                              "Update Company",
                              class = "btn-success"),
                 selectInput("company",
                             "Pick a company",
                             choices = NULL),
                 inputPanel(
                   numericInput(
                     "SMA1",
                     "SMA 1",
                     min = 0,
                     max = 200,
                     value = 5
                   ),
                   numericInput(
                     "SMA2",
                     "SMA 2",
                     min = 0,
                     max = 200,
                     value = 20
                   ),
                   numericInput(
                     "EMA1",
                     "EMA 1",
                     min = 0,
                     max = 200,
                     value = 50
                   ),
                   dateRangeInput(
                     "chart_date_range",
                     "Date Range for Plot",
                     start = Sys.Date() - 30,
                     end = Sys.Date() + 1
                   )
                 )
                 
               )
               
             ),
             mainPanel(tabsetPanel(
               tabPanel("Chart", plotOutput("chart_series")),
               tabPanel("Interactive", plotlyOutput("interactive_plot")),
               tabPanel("Table", reactableOutput("trend_data"))
             ))
           )),
  tabPanel(title = "Stock Analyzer",
           fluidPage(
             sidebarLayout(
               titlePanel("Type any stock and get up to date Financial Information"),
               sidebarPanel(
                 textInput("comp", label = "Type a company ticker", placeholder = "AAPL"),
                 actionButton("update_single_comp", label = "Change Company"),
                 inputPanel(
                   numericInput(
                     "single_SMA1",
                     "SMA 1",
                     min = 0,
                     max = 200,
                     value = 5
                   ),
                   numericInput(
                     "single_SMA2",
                     "SMA 2",
                     min = 0,
                     max = 200,
                     value = 20
                   ),
                   numericInput(
                     "single_EMA1",
                     "EMA 1",
                     min = 0,
                     max = 200,
                     value = 50
                   ),
                   dateRangeInput(
                     "single_chart_date_range",
                     "Date Range for Plot",
                     start = Sys.Date() - 30,
                     end = Sys.Date() + 1
                   ),
                   selectInput(
                     "single_fin_values",
                     "Select Financial Values",
                     multiple = TRUE,
                     choices = NULL,
                     selected = default_fin
                   )
                 )
                 
               )
               
             ),
             mainPanel(tabsetPanel(
               tabPanel("Chart", plotOutput("single_chart")),
               tabPanel("Interactive", plotlyOutput("single_interactive_plot")),
               tabPanel("Daily Table", reactableOutput("single_trend_data")),
               tabPanel("Fundamental", reactableOutput("single_fundamentals"))
             ))
           ))
)
