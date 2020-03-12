library("shiny")
library("ggplot2")
library("ggvis")
library("plotly")
library("DT")
library("tidyr")
library("stringr")
source("analysis.R")

state_codes <- read.csv('state_codes.csv', stringsAsFactors = FALSE)

#group the dataset by relevant columns
opioid_df_long <- opioid_df %>%
  select(State, Year, Multiple.Cause.of.death, Deaths) %>%
  group_by(State, Multiple.Cause.of.death, Year)

#spread data in wide version in order to add Multiple.Cause.of.death into three categories
opioid_df_wide <- spread(
  opioid_df_long,
  key = Multiple.Cause.of.death,
  value = Deaths
)

#set NA value in the data to be 0
opioid_df_wide[is.na(opioid_df_wide)] <- 0

#generate the three categories of drugs and select those three categories and State, Year columns
q1_plot_df <- opioid_df_wide %>%
  mutate(Heroin = Heroin, `illegal drugs` = `Other and unspecified narcotics`+ `Other synthetic narcotics` + Heroin, 
         `legal drugs` = Methadone + `Other opioids`) %>%
  select(State, Year, Heroin, `illegal drugs`, `legal drugs`)

#Leftjoin the dataset with state codes
joined_data <- left_join(q1_plot_df, state_codes, by = "State")

#Data nalysis question
max_illegal_2018_rank <- joined_data %>%
  filter(Year == "2018") %>%
  arrange(-`illegal drugs`)

max_illegal_2018_state <- max_illegal_2018_rank[1,]$State
max_illegal_2018_number <- max_illegal_2018_rank[1,]$`illegal drugs`


#A function that takes users' two inputs: "drug category" and "state" and returns a drug death count line graph
my_line <- function(data, dc.var = "", st.var = "") {
  data <- data %>% 
    select(State, Year, dc.var) %>% 
    filter(State == st.var) #select useful columns and filter specific state input by users
  p <- plot_ly(x = data$Year, 
               y = data[[dc.var]],
               type = 'scatter', mode = 'lines') %>%
    layout(xaxis = list(title = "Year"),
           yaxis = list(title = "Death counts"),
           title = paste0(st.var, " deaths caused by ", dc.var, " each year"))
  return(p)
}

#A function that takes users' two inputs: "drug category" and "year" and returns a U.S drug death count map
my_map <- function(data, dc.var, yr.var) {
  l <- list(color = toRGB("white"), width = 2)
  g <- list(
    scope = 'usa', 
    projection = list(type = 'albers usa'),
    showlakes = TRUE,
    lakecolor = toRGB('white')
  )
  var.equation <- paste0('~', dc.var) # Make equation for map color / text
  data <- data %>%
    filter(Year == yr.var) #filter specific year input by user
  p <- plot_geo(data, locationmode = 'USA-states') %>%
    add_trace(
      z = data[[dc.var]], text = ~State, locations = ~code,
      color = data[[dc.var]], colors = 'Purples'
    ) %>%
    colorbar(title = "Number of people") %>% #set the colorbar
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
  
  # Receive the input from tab of question 2, re-filtering data for map, table, and line graph
  slider2<-reactive({input$year2})
  opioid_df_map2_slider <- eventReactive(slider2(), {opioid_df %>% 
      filter(Year >= slider2()[1] & Year <= slider2()[2]) %>% 
      group_by(State) %>% 
      summarize("Total_Death" = sum(Deaths, na.rm = T)) %>% 
      mutate(change_label =cut(Total_Death, breaks = c(0,100,500,1000,5000,10000,15000,20000), labels = c("0-100","100-500","500-1000","1000-5000","5000-10000","10000-15000","15000-20000")))
  })
  
  opioid_df_line2_slider_all <- eventReactive(slider2(), {opioid_df %>%
      group_by(Year) %>%
      summarize("Total_Death" = sum(Deaths, na.rm = T)) %>%
      mutate(State = "Overall")
  })
  
  opioid_df_line2_slider <- eventReactive(slider2(), {opioid_df %>% 
      filter(Year >= slider2()[1] & Year <= slider2()[2]) %>%
      group_by(State, Year) %>%
      summarize("Total_Death" = sum(Deaths, na.rm = T)) %>%
      rbind.data.frame(opioid_df_line2_slider_all())
  })
  
  # Render the table for question 2
  output$table2 <- renderTable({
    df <- opioid_df_map2_slider()
    df <- df %>% 
      select(State, Total_Death)
    return(df)
  })
  
  # Render the line for question 2 using plotly
  output$line2 <- renderPlotly({
    if (input$state_line == "Overall") {
      opioid_line(opioid_df_line2_slider_all(), input$state_line)
    } else {
      opioid_line(opioid_df_line2_slider(), input$state_line)
    }
  })
  
  # Render the map for question 2 using ggplot
  output$map2 <- renderPlot({
    df <- opioid_df_map2_slider()
    df$State <- tolower(df$State)
    states_map <- map_data("state")
    opioid_map <- ggplot()
    opioid_map <- opioid_map + geom_map(data = states_map, map = states_map,
                                        aes(x = long, y = lat, map_id = region),
                                        fill = "#ffffff", color = "#000000")
    opioid_map <- opioid_map + geom_map(data = df, map = states_map,
                                        aes(fill = change_label, map_id = State),
                                        color="#000000")
    opioid_map <- opioid_map + scale_fill_brewer(palette = "Reds")
    opioid_map <- opioid_map + labs(x = NULL, y = NULL)
    opioid_map <- opioid_map + theme(axis.ticks = element_blank()) +
      theme(axis.text = element_blank()) + 
      theme(panel.border = element_blank()) +
      theme(panel.background = element_blank()) +
      coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
      labs(title = paste0("Total Death Caused by Opioid Overdose from ", slider2()[1], " to ", slider2()[2]))
    return(opioid_map)
  })
  
  
  # The plotly function for generating line graph for question 2
  opioid_line <- function(data, st.var = "") {
    data <- data %>%
      filter(State == st.var)
    p <- plot_ly(x = data$Year, 
                 y = data$Total_Death,
                 type = 'scatter', mode = 'lines') %>%
      layout(xaxis = list(title = "Year"),
             yaxis = list(title = "Number of Deaths"),
             title = paste0(st.var, " death count caused by Opioid Overdose"))
    return(p)
  }
  
  filter3 <- function(df){
    if(input$type != "Both"){
      if(input$type == "Total deaths"){
        df <- drug_medi_df_long %>% 
          filter(Type == "Total_death")
      }else{
        df <- drug_medi_df_long %>% 
          filter(Type == "spending")
      }
    }
    return(df)
  }
  
  labels33 <- function(){
    if(input$type == "Both"){
      labell <- c("Medicaid Spending(in hundred million $US)", "Total Death Count")
    }else if(input$type == "Total deaths"){
      labell <- c("Total Death Count")
      }else{
        labell <- c("Medicaid Spending(in hundred million $US)")
      }
    return(labell)
  }
  
  
  #Rendering a graph for third question
  output$plot3 <- renderPlot({
    df <- filter3(drug_medi_df_long)
    labels3 <- labels33()
    z <- ggplot(data = df, mapping = aes(x = Year, y = value, color = Type ) ) +
      geom_point() +
      geom_line()+
      labs(title = paste0("Medicaid Spending by US Government and Total Drug Overdose death from ", slider()[1], " to " , slider()[2]), x = "Year",y = "Values")+
      scale_color_discrete(name = "Type", label = labels3)+
      theme(legend.position = c(0.25, 0.85))+
      scale_x_continuous(limits = input$year3)
    return(z)
  })
  
  #Rescaling the data
  drug_medi_df$spending <- drug_medi_df$spending*100000000
  #renaming the columns
  names(drug_medi_df)[3] <- "Medicaid_Spending"
  
  #Records user activity on the slider input
  slider<-reactive({input$year3})

  #Filters the dataframes according to user interaction with slider
  drug_medi_df_fil <- eventReactive(slider(), {drug_medi_df %>% 
      filter(Year >= slider()[1] & Year <= slider()[2])})
    
  
  #Rendering the filtered dataframe
  output$table3 <- renderTable(drug_medi_df_fil(), digits = 0)

}