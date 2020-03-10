library("shiny")
library("ggplot2")
library("ggvis")
library("plotly")
library("DT")
source("analysis.R")


server <- function(input, output) {
  
  output$plot2 <- renderPlot({
    #filter the dataset
    opioid_df_map_slider <- opioid_df %>%
      group_by(State) %>%
      filter(Year <= input$year) %>%
      summarize("Total Death" = sum(Deaths, na.rm = T))
    opioid_df_map_slider$change_label <- cut(opioid_df_map_slider$`Total Death`, breaks = c(0,100,500,1000,5000,10000,15000,20000), labels = c("0-100","100-500","500-1000","1000-5000","5000-10000","10000-15000","15000-20000"))
    
    # return the plot
    states_map <- map_data("state")
    opioid_map <- ggplot()
    opioid_map <- opioid_map + geom_map(data = states_map, map = states_map,
                                        aes(x = long, y = lat, map_id = region),
                                        fill = "#ffffff", color = "#000000")
    opioid_map <- opioid_map + geom_map(data = opioid_df_map_slider, map = states_map,
                                        aes(fill = change_label, map_id = State),
                                        color="#000000")
    opioid_map <- opioid_map + scale_fill_brewer(palette = "Reds")
    opioid_map <- opioid_map + labs(x = NULL, y = NULL)
    opioid_map <- opioid_map + theme(axis.ticks = element_blank()) +
      theme(axis.text = element_blank()) + 
      theme(panel.border = element_blank()) +
      theme(panel.background = element_blank()) +
      coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
      labs(title = "Total Death Caused by Opioid Overdose in Each State From 1999-")
    opioid_map
  })
}