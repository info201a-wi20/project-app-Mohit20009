library(shiny)
library(plotly)
library("dplyr")

opioid_df <- read.csv("Multiple Cause of Death, 1999-2018.csv")
all_states <- opioid_df$State
unique_states <- unique(all_states)
all_year <- opioid_df$Year
unique_year <- unique(all_year)

opioid_df <- read.csv("Multiple Cause of Death, 1999-2018.csv")
all_states <- opioid_df$State
unique_states <- unique(all_states)
all_year <- opioid_df$Year
unique_year <- unique(all_year)


trend_graph_panel <- tabPanel("Death count trend graph", fluid = TRUE,
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(
                            inputId = "drugcate",
                            label = "Drug categories",
                            choices = c("Heroin", "legal drugs", "illegal drugs")
                          ),
                          selectInput(
                            inputId = "state",
                            label = "State",
                            choices = unique_states
                          )
                        ),
                        mainPanel(
                          plotlyOutput("line_graph")    
                        )  
                      )
                     )

map_panel <- tabPanel("Death count map", fluid = TRUE,
              sidebarLayout(
                sidebarPanel(
                  selectInput(
                    inputId = "drugcate2",
                    label = "Drug categories",
                    choices = c("Heroin", "legal drugs", "illegal drugs")
                  ),
                  selectInput(
                    inputId = "year",
                    label = "Year",
                    choices = unique_year
                  )
                ),
                mainPanel(
                  plotlyOutput("map")
                )
              )
             )

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
  p("In order to get a thorough analysis of the question, it is neccessary to show..........."),
  h4("Analysis:"),
  p("The trending graph the map have given a similar result that in general, the death caused by illegal drugs is more unpredictable and therefore harder to control than the legal drugs.")
)

ui <- navbarPage(
        title = "",
        drug_death_count_panel
      )
