library(plotly)
library(stringr)
library("dplyr")
library(tidyr)


setwd("~/project-app-Mohit20009")

opioid_df <- read.csv("Multiple Cause of Death, 1999-2018.csv", stringsAsFactors = FALSE)
state_codes <- read.csv('state_codes.csv', stringsAsFactors = FALSE)

opioid_df_long <- opioid_df %>%
  select(State, Year, Multiple.Cause.of.death, Deaths) %>%
  group_by(State, Multiple.Cause.of.death, Year)

opioid_df_wide <- spread(
  opioid_df_long,
  key = Multiple.Cause.of.death,
  value = Deaths
)

opioid_df_wide[is.na(opioid_df_wide)] <- 0

q1_plot_df <- opioid_df_wide %>%
  mutate(Heroin = Heroin, `illegal drugs` = `Other and unspecified narcotics`+ `Other synthetic narcotics` + Heroin, 
         `legal drugs` = Methadone + `Other opioids`) %>%
  select(State, Year, Heroin, `illegal drugs`, `legal drugs`)

joined_data <- left_join(q1_plot_df, state_codes, by = "State")


my_line <- function(data, dc.var = "", st.var = "") {
  data <- data %>% 
    select(State, Year, dc.var) %>% 
    filter(State == st.var)
  p <- plot_ly(x = data$Year,
               y = data[[dc.var]],
               type = 'scatter', mode = 'lines') %>%
    layout(xaxis = list(title = "Year"),
           yaxis = list(title = "Death counts"),
           title = paste0(st.var, " deaths caused by ", dc.var, " each year"))
  return(p)
}


my_map <- function(data, dc.var, yr.var) {
  l <- list(color = toRGB("white"), width = 2)
  g <- list(
    scope = 'usa',
    projection = list(type = 'albers usa'),
    showlakes = TRUE,
    lakecolor = toRGB('white')
  )
  var.equation <- paste0('~', dc.var)
  data <- data %>%
    filter(Year == yr.var)
  p <- plot_geo(data, locationmode = 'USA-states') %>%
    add_trace(
      z = data[[dc.var]], text = ~State, locations = ~code,
      color = data[[dc.var]], colors = 'Purples'
    ) %>%
    colorbar(title = "Number of people") %>%
    layout(
      title = paste0(yr.var, " U.S deaths caused by ", dc.var, " map"),
      geo = g
    )
  return(p)
}
  

server <- function(input, output) {
  output$line_graph <- renderPlotly({ 
    return(my_line(q1_plot_df, input$drugcate, input$state))
  })
  output$map <- renderPlotly({
    return(my_map(joined_data, input$drugcate2, input$year))
  })
}



  
