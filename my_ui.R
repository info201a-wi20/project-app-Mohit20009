library(shiny)
library(plotly)
library("dplyr")

#data 
opioid_df <- read.csv("Multiple Cause of Death, 1999-2018.csv")

#take out the state names from data frame, for use as users' choices
all_states <- opioid_df$State
unique_states <- unique(all_states)

#take out the years from data frame, for use as users' choices
all_year <- opioid_df$Year
unique_year <- unique(all_year)


#line graph panel as first tabPanel
trend_graph_panel <- tabPanel(h3("Death count trend graph"), fluid = TRUE,
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(
                            inputId = "drugcate", #assign inputId
                            label = "Drug categories",
                            choices = c("Heroin", "legal drugs", "illegal drugs")
                          ),
                          textInput(
                            inputId = "state", #assign inputId
                            label = "Find a state",
                            value = ""
                          )
                        ),
                        mainPanel(
                          plotlyOutput("line_graph") #draw the line graph
                        )  
                      )
                     )

#map panel as another tabPanel
map_panel <- tabPanel(h3("Death count map"), fluid = TRUE,
              sidebarLayout(
                sidebarPanel(
                  selectInput(
                    inputId = "drugcate2", #assign inputId
                    label = "Drug categories",
                    choices = c("Heroin", "legal drugs", "illegal drugs")
                  ),
                  selectInput(
                    inputId = "year", #assign inputId
                    label = "Year",
                    choices = unique_year
                  )
                ),
                mainPanel(
                  plotlyOutput("map") #draw the map
                )
              )
             )

#The main tabPanel including the two sub-tabPanels and all the texts
drug_death_count_panel <- tabPanel(
  title = "Drug death count and drug categories",
  titlePanel("What is the drug death count by prescription and illegal drugs from 1999-2018?"),
  tabsetPanel(
    trend_graph_panel,
    map_panel
  ),
  h4("Aim:"),
  p("This would tell us the history about the opioid crisis, and the deaths due to different drug overdoses. 
   Helping us identify the most dangerous drug and can be used to formulate policies around that drug. 
   The Opioid dataset contains all the deaths from drug overdose between 1999-2018."),
  h4("Approach:"),
  p("In order to answer the question of death count by prescription and illegal drugs, 
    we have to first define some terms which are useful in further analysis."),
  p("For the multiple types of drugs, it is neccessary to classify them into three major categories: 
    the illegal drugs, legal drugs and Heroin. The legal drugs include 'Methadone' and 'other opioids' listed in the dataset. 
    The illegal drugs include 'Other and unspecified narcotics', 'other synthetic narcotics' and 'Heroin'. 
    Heroin, which is a major well-known illegal drug, is also listed seperately as a reference. For instance, 
    'Other opioids' is a drug type, 'legal drugs' is a drug category, while 'Heroin' is both a drug type and category."),
  p("In order to get a thorough analysis of the question, it is neccessary to show the data from the span of both time and 
    geomtric region. A trending line graph and a map would be good choices to approach this question."),
  h4("Analysis:"),
  p("The trending line graph shows each state death count caused by each drug category from 1999 to 2018.(In some states,
    the data is missing for some years) The map shows U.S death count caused by each drug category in specific year. 
    By viewing both graphs, we can make conclusions about death count by the three categories in different year and differnet 
    states. For example, we can tell that in 2018, New York State had the highest death number caused by Heroin.")
)

#Ui with navbarPage
ui <- navbarPage(
        title = "",
        drug_death_count_panel
      )
