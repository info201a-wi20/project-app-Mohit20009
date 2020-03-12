library("shinyWidgets")
library("plotly")

# Creating the slider for the time range from 1999 to 2018
slider_opioid <- sidebarPanel(
  sliderInput(inputId = "year", label = "Year", min = 1999, max = 2018, value = c(1999,2018), sep = "")
)

main_content_opioid_death <- mainPanel(
  # Draw the map, line graph, and table in opioid_death_panel tab
  tabsetPanel(
    tabPanel("Map", plotOutput("map2")),
    tabPanel("Line Graph", sidebarLayout(sidebarPanel(selectInput(inputId = "state_line", label = "State", choices = c("Overall",state.name))),mainPanel(plotlyOutput("line2")))),
    tabPanel("Table", tableOutput("table2"))
    
  )
)

# Create the tab for question
opioid_death_panel <- tabPanel(
  title = "Death Caused by Opioid Overdose",
  titlePanel("The number of people dead by Overdosing Opioids in the US"),
  
  sidebarLayout(
    main_content_opioid_death,
    slider_opioid
  ),
  h4("Aim:"),
  p("We are trying to find states that suffers the most from opioids, and figure out which time period the death number jumps."),
  h4("Approach:"),
  p("In this part we added the total number of death caused by heroin, methadone, and other kinds of opioids for all counties in each state from 1999-2018 together, so that we can get a total number of death caused by opioid overdose in each state, from 1999-2018. We also calculated the total population of each state to get a more clear picture for the death rate."),
  h4("Analysis:"),
  p("After analysis, we created a map, a line graph, and a table to show the time counts for each state along the time."),
  p("In statistics, we can see California, New York, Florida, Ohio, Texas and Washington shows the most death caused by opioid overdose over time, and there is a jump of death number for almost all states in the 2000s"),
  p("These result shows that these top 6 states are the states that suffers from the opioid problem the most, but we can see that they also have the largest population as well. Therefore, we cannot conclude that these states are the worst performing states in the opioid crisis. However, these states still need to reinforce the polices as they have large population and people can easily become drug addicts."),
  p("In the 2000s, the government took steps to limit the supply of precription pills, and authorities tightened limits on painkillers. Americans who were already hooked started to find more dangerous drugs, like heroins, which caused the jump of death number of opioid overdose.")
)


ui<- navbarPage(
  title = "",
  opioid_death_panel,
  theme = "style.css"
)
