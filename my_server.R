library("shiny")
library("ggplot2")
library("ggvis")
library("plotly")
library("DT")
source("analysis.R")


server <- function(input, output) {
  
  output$plot3 <- renderPlot({
    # return the plot
    ggplot(data = drug_medi_df_long, mapping = aes(x = Year, y = value, color = Type) ) +
      geom_point(aes()) +
      geom_line()+
      labs(title = "Medicaid Spending by US Government and Total Drug Overdose death(1999-2014)", x = "Year",y = "Values")+
      scale_color_discrete(name = "Type", label = c("Medicaid Spending(in hundred million $US)", "Total Death Count"))+
      theme(legend.position = c(0.25, 0.85))+
      scale_x_continuous(limits = input$year)
  })
  drug_medi_df$spending <- drug_medi_df$spending*100000000
  names(drug_medi_df)[3] <- "Medicaid_Spending"
  
  slider<-reactive({input$year})
  drug_medi_df_fil <- eventReactive(slider(), {drug_medi_df %>% 
      filter(Year >= slider()[1] & Year <= slider()[2])})
  output$table <- renderTable(drug_medi_df_fil(), digits = 0)
}

xyz <- drug_medi_df %>% 
  filter(Year >= 1999 & Year <= 2012)

z <- datatable(xyz)%>% 
  formatStyle(columns = c('Year', 'Total_death', 'spending'),color = 'white')
print(z)
