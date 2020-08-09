library(shiny)

library(COVID19)
library(data.table)
library(dplyr)
library(stringr)
library(lubridate)
library(glue)
library(RcppRoll)
library(lubridate)
library(glue)
library(plotly)
library(tigris)
library(leaflet)
library(fst)
states_shp <- readRDS("data/states.RDS") 

states_map <- leaflet(states_shp) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(fillColor = "white",
              color = "black",
              weight = 0.5, popup = ~NAME, label = ~NAME) %>%
  setView(-98.5795, 39.8282, zoom=4)

data <- read.fst('data/data.fst')
data_long <- read.fst('data/data_long.fst')

state_data <- read.fst("data/state_data.fst")
state_data_long <- read.fst("data/state_data_long.fst")

covid_plot_data <- function(dataset, variables, date_start = '2020-01-22', date_end = Sys.Date() - days(2), state = NULL) {
  stopifnot(dataset %in% c("national", "state"))
  
  if (dataset == "national") {
    data_long %>% 
      filter(between(date, as.Date(date_start), as.Date(date_end)),
             type %in% variables)
      
  } else {
    stopifnot(!is.null(state))
    
    state_data_long %>% 
      filter(State == state, between(date, as.Date(date_start), as.Date(date_end)),
             type %in% variables)
  }
}
 
pretty_names <- c("confirmed" = "Total Cases", "daily_cases" = "Daily Cases", 
                  "daily_deaths" = 'Daily Deaths', "daily_tests" = 'Daily Tests', 
  "deaths" = 'Total Deaths', "rolling_weekly_cases" = 'Weekly Rolling Cases', 
  "rolling_weekly_deaths" = 'Weekly Rolling Deaths', "rolling_weekly_tests" = 'Weekly Rolling Tests', 
  "tests" = "Tests")

pretty_names_select <- names(pretty_names)
names(pretty_names_select) <- pretty_names

state_names <- sort(unique(state_data$State))

covid_plot <- function(.data, variables) {
  
  if('State' %in% names(.data)) {
  state_name <- .data$State[[1]]
  title <- glue("{state_name}: {paste0(pretty_names[variables], collapse = ', ')}")
  .data <- .data %>% 
    mutate(Text = glue("State: {State}\nDate: {date}\n{pretty_names[type]}: {prettyNum(value, big.mark=',')}"))
  
  plotly::plot_ly(data = .data) %>% 
    plotly::add_trace(x = ~date, y = ~value, color = ~type, mode = 'lines+markers',
                      text = ~Text,
                      hoverinfo = 'text') %>% 
    plotly::layout(title = title,
                   xaxis = list(title = 'Date'),
                   yaxis = list(title = 'Count'))
  } else {
    title <- glue("United States: {paste0(pretty_names[variables], collapse = ', ')}")
    .data <- .data %>% 
      mutate(Text = glue("Date: {date}\n{pretty_names[type]}: {prettyNum(value,  big.mark=',')}"))
    plotly::plot_ly(data = .data) %>% 
      plotly::add_trace(x = ~date, y = ~value, color = ~type, mode = 'lines+markers',
                        text = ~Text, hoverinfo = 'text') %>% 
      plotly::layout(title = title, xaxis = list(title = 'Date'),
                     yaxis = list(title = 'Count'))
  }
    
}



