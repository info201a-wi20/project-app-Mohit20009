library("shiny")
library("ggplot2")
library("ggvis")
library("plotly")
library("DT")
source("analysis.R")


server <- function(input, output) {
  #Rendering a graph for third question
  output$plot3 <- renderPlot({
    ggplot(data = drug_medi_df_long, mapping = aes(x = Year, y = value, color = Type) ) +
      geom_point() +
      geom_line()+
      labs(title = "Medicaid Spending by US Government and Total Drug Overdose death(1999-2014)", x = "Year",y = "Values")+
      scale_color_discrete(name = "Type", label = c("Medicaid Spending(in hundred million $US)", "Total Death Count"))+
      theme(legend.position = c(0.25, 0.85))+
      scale_x_continuous(limits = input$year)
  })
  
  #Rescaling the data
  drug_medi_df$spending <- drug_medi_df$spending*100000000
  #renaming the columns
  names(drug_medi_df)[3] <- "Medicaid_Spending"
  
  #Records user activity on the slider input
  slider<-reactive({input$year})
  
  #Filters the dataframes according to user interaction with slider
  drug_medi_df_fil <- eventReactive(slider(), {drug_medi_df %>% 
      filter(Year >= slider()[1] & Year <= slider()[2])})
  
  #Rendering the filtered dataframe
  output$table3 <- renderTable(drug_medi_df_fil(), digits = 0)
}