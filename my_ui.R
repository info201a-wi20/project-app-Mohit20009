library("shinyWidgets")

slider_opioid <- sidebarPanel(
  # Creating the slider for the range from 1999 to 2018
  sliderInput(inputId = "year", label = "Year", min = 1999, max = 2018, value = c(1999,2018), sep = "")
)

main_content_opioid_death <- mainPanel(
  # A `plotOutput()` element showing the 'plot' output (defined in the server)
  tabsetPanel(
    tabPanel("Plot", plotOutput("plot2")), 
    tabPanel("Table", tableOutput("table2"))
    
  )
)
opioid_death_panel <- tabPanel(
  title = "Death Caused by Opioid Overdose",
  titlePanel("The number of people dead by Overdosing Opioids in the US"),
  
  sidebarLayout(
    main_content,
    slider_opioid
  ),
)


ui<- navbarPage(
  title = "",
  opioid_death_panel,
  theme = "style.css"
)