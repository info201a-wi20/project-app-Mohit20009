library("shiny")
library("ggplot2")
library("ggvis")
library("plotly")
library("DT")
source("analysis.R")



server <- function(input, output) {
  
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
      labs(title = paste0("Total Death Caused by Opioid Overdose in Each State From ", slider2()[1], " to ", slider2()[2]))
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
             yaxis = list(title = "Death for Each Year"),
             title = paste0(st.var, " Death Caused by Opioid"))
    return(p)
  }
  
  
  
  
  #Rendering a graph for third question
  output$plot3 <- renderPlot({
    ggplot(data = drug_medi_df_long, mapping = aes(x = Year, y = value, color = Type) ) +
      geom_point() +
      geom_line()+
      labs(title = "Medicaid Spending by US Government and Total Drug Overdose death(1999-2014)", x = "Year",y = "Values")+
      scale_color_discrete(name = "Type", label = c("Medicaid Spending(in hundred million $US)", "Total Death Count"))+
      theme(legend.position = c(0.25, 0.85))+
      scale_x_continuous(limits = input$year3)
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