library("shinyWidgets")

slider_opioid <- sidebarPanel(
  # Creating the slider for the range from 1999 to 2018
  sliderInput(inputId = "year", label = "Year", min = 1999, max = 2018, value = 2018, sep = "")
)

main_content <- mainPanel(
  # A `plotOutput()` element showing the 'plot' output (defined in the server)
  tabsetPanel(
    tabPanel("Plot", plotOutput("plot2")), 
    tabPanel("Table", tableOutput("table"), setBackgroundColor(color = "white"))
    
  )
)
drug_medicaid_panel <- tabPanel(
  title = "Death Caused by Opioid Overdose",
  titlePanel("The number of people dead by Overdosing Opioids in the US"),
  
  sidebarLayout(
    main_content,
    slider_opioid
  ),
)


ui<- navbarPage(
  title = "",
  drug_medicaid_panel,
  theme = "style.css"
)