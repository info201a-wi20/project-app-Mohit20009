library("shinyWidgets")
library("shiny")
library("plotly")
library("dplyr")
source("my_server.R")

#take out the years from data frame, for use as users' choices
all_year <- opioid_df$Year
unique_year <- unique(all_year)

#take out the state names from data frame, for use as users' choices
all_state <- opioid_df$State
unique_state <- unique(all_state)

#line graph panel as first tabPanel
trend_graph_panel <- tabPanel("Death count trend graph", fluid = TRUE,
                              sidebarLayout(
                                sidebarPanel(
                                  selectInput(
                                    inputId = "drugcate", #assign inputId
                                    label = "Drug categories",
                                    choices = c("Heroin", "legal drugs", "illegal drugs")
                                  ),
                                  selectInput(
                                    inputId = "state", #assign inputId
                                    label = "Choose a state",
                                    choices = unique_state
                                  )
                                ),
                                mainPanel(
                                  plotlyOutput("line_graph") #draw the line graph
                                )  
                              )
)

#map panel as another tabPanel
map_panel <- tabPanel("Death count map", fluid = TRUE,
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
  title = "Drug death count & drug categories",
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
    states. For example, we can tell that in 2018, ", strong(max_illegal_2018_state), " state had the highest death number caused by illegal
    drugs, of ", strong(max_illegal_2018_number), " people."),
  p("In general, the trending of legal drug deaths keeps a constant increasing from 1999 to 2011, then following a little drop down or keeps growing 
    with slower pace in most states. At the same time the deaths count of Heroin and illegal drugs drastically increased. This pattern is especially 
    obvious in states with a serious drug deaths problem such as New York State and California, and also typical in Florida, a state with relatively 
    low deaths caused by Heroin and illegal drugs before 2011, but exceeds the data of California on death caused by illegal drugs in 2017.")
)

#Creating Home Tab
home_panel <- tabPanel(
  title = "Home",
  #Header
  h1("Study on the Opioid Crisis and Medicaid Spending in the United States"),
  #Introduction
  p("Our project will be analyzing data on opioid crisis and Medicaid spending in the United States. Opioids are drugs primarily used in the medical field as a pain reliever, treat cancer patients, anesthesia, and more. There are different types of opioid drugs that are classified as legal or illegal in the United States. Doctors have to prescribe legal drugs like oxycodone, hydrocodone, morphine, and others. Illegal drugs include heroin, synthetic opioids - fentanyl, and more. All these drugs are highly potent, which means even a small amount of the drug can lead to death if not supervized by medical experts. In the 1990s pharmaceutical companies began falsely advertising legal opioid drugs as non-addictive and bribed doctors to often prescribe their patients these drugs. People got hooked to these drugs and often overdosed, which led to an increase in deaths. In 2011, the US government sued pharmaceutical companies and prosecuted doctors which led to leass availability of legal opioids. People who were addicted to these drugs switched to more dangerous illegal drugs like Heroin. The number of deaths from opioid drug overdose has been increasing and this has been termed as the opioid crisis. US government has been spending billions of dollar on Medicaid, law enforcement, and more to fight the crisis. Medicaid is a fedreal and state program that allocates financial help to people towards medical cost like medical products, treatment, and more."),
  p("This analysis would give an insight to the policymakers and the concerned people on how the crisis has worsened over time. It will also shed light on the current situation, and show the impact of Medicaid spending, while also examining if there is a need for other stronger drug policies to deal with the crisis.")
)


# Creating the slider for the time range from 1999 to 2018


main_content_opioid_death <- mainPanel(
  # Draw the map, line graph, and table in opioid_death_panel tab
  tabsetPanel(
    tabPanel("Line Graph", sidebarLayout(mainPanel(plotlyOutput("line2")),sidebarPanel(selectInput(inputId = "state_line", label = "State", choices = c("Overall",state.name))))),
    tabPanel("Map", fluidRow(
      column(8,
             (plotOutput("map2"))),
      column(4,
             sliderInput(inputId = "year2", label = "Year", min = 1999, max = 2018, value = c(1999,2018), sep = ""))) 
    ) 
    #tabPanel("Table", sidebarLayout(slider_opioid, tableOutput("table2")))
  ), width =12
)

# Create the tab for question
opioid_death_panel <- tabPanel(
  title = "States & Deaths by Opioid Overdose",
  h2("The number of people dead by Overdosing Opioids in each state"),
  main_content_opioid_death,
  br(),
  h4("Aim:"),
  p("To find states that suffers the most from opioids crisis and needs urgent help"),
  h4("Approach:"),
  p("In this part we added the total number of death caused by heroin, methadone, and other kinds of opioids for all counties in each state from 1999-2018 together, so that we can get a total number of death caused by opioid overdose in each state, from 1999-2018. We also calculated the total population of each state to get a more clear picture for the death rate."),
  h4("Analysis:"),
  p("After analysis, we created a map, a line graph, and a table to show the time counts for each state along the time."),
  p("We found that", strong("California, New York, Florida, Ohio, Texas"), "and", strong("Washington"), "shows the most death caused by opioid overdose over time. These result shows that these top 6 states are the states that suffers from the opioid problem the most, but we can see that they also have the largest population as well. Therefore, we cannot conclude that these states are the worst performing states in the opioid crisis. However, these states still need to reinforce the polices as they have large population and people can easily become drug addicts.")
)



#widget slider input to select the range of years
sidebar_content <- sidebarPanel(
  sliderInput(inputId = "year3", label = "Year", min = 1999, max = 2014, value = c(1999, 2014), sep = ""),
  selectInput(
    inputId = "type", #assign inputId
    label = "Type",
    choices = c("Both", "Total deaths", "Medicaid Spending")
  )
)

#Layout for Question 3
main_content3 <- mainPanel(
  tabsetPanel(
    
    #Rendering the Plot for Question3
    tabPanel("Plot", plotOutput("plot3")), 
    
    #Rendering the Table for Question3
    tabPanel("Table", tableOutput("table3"))
  )
)

#Tab for Question3
drug_medicaid_panel <- tabPanel(
  title = "Drug Overdose & Medicaid Spending",
  h2("Did Medicaid spending help reduce total deaths from drug overdose?"),
  
  #Adding widgets and layout
 sidebarLayout(
    main_content3,
    sidebar_content
  ),
  #Answering the question
  h4("Aim:"),
  p("To find if Medicaid is a successful way to curb the crisis or is it an unnecessary expenditure of the tax payers money."),
  h4("Approach:"),
  p("In order to analyze the opioid data we selected the varibles Year and Deaths. Consequntly, to sum up all the deaths of each year we used group by Year and summarize function. Futhermore, we also analysed the dataframe containing the Medicaid spending, we filtered the data by Region_Name to be United States and Item to be Medicaid/Personal Health Care (Millions of Dollars) and selected the necessary years. After performing data wrangling, we combined the two dataframes to answer the question."),
  h4("Analysis:"),
  p("We correlated overdose death with time, medicaid spending with time, and overdose death  with medicaid spending. Having the correlation value of overdose death compared to time as", strong(death_cor), ", we consider it to be growing linearly. We also dervied a strong positive correlation between medicaid spending and time to be", strong(medicaid_cor),"."),
  p("We intially reckoned that if there is a strong positive correlation between overdose deaths and medicaid spending, then medicaid spending is effecient in reducing the overdose deaths. The relationship should be negative or weak(between 0.5 to -0.5) to show that the number of deaths are reducing with more medicaid spending. To our surprise, we found that our intial belief held. There was a strong positive relationship of", strong(death_medi_cor), "which confirms that the medicaid spending is not helping with the drug crisis."),
  p("Whilst medicaid spending is benefical to a lot of Americans, however it has a very little to no effect on drug addicts. Since, there is only a fraction of medicaid spending allocated to drug addicts,the US government should reconsider to invest this money in other programs and policies. One can notice how the action taken by the US government against pharmaceutical companies and changes made in drug policies help reduce the total overdose deaths between 2011-2012. The government can replicate similar drug policies and actions, while enforcing strong laws against illegal drugs.")
)

#Tab for about the team
about_panel <- tabPanel(
  title = "Team",
  h2("Our Team", align = "center"),
  
  #Layout  using the 12grid system in fluidRow
  fluidRow(
    column(3),
    #About Mohit
    column(2,
           div(class="panel panel-default", 
               div(class="panel-body",  width = "1000px",
                   align = "center",
                   div(
                     tags$img(src = "mohit.jpg", 
                              width = "50px", height = "50px")
                   ),
                   div(
                     tags$h5("Mohit Jain"),
                     tags$h6( tags$i("UW Student"))
                   ),
                   div(
                     "Aspiring to become a data scientist"
                   ),
                   div(
                     a(
                       # where to link
                       href = "https://www.linkedin.com/in/mohit-jain-a2a2a5173/",
                       # what text to show
                       "LinkedIn Profile"
                     )
                   )
               )
           )
    ),
    #About Jin
    column(2,
           div(class="panel panel-default", 
               div(class="panel-body",  width = "1000px",
                   align = "center",
                   div(
                     tags$img(src = "jin.jpg", 
                              width = "50px", height = "50px")
                   ),
                   div(
                     tags$h5("Jin Zhang"),
                     tags$h6( tags$i("UW Student"))
                   ),
                   div(
                     "Aspiring to become a data scientist"
                   ),
                   div(
                     a(
                       # where to link
                       href = "https://www.linkedin.com/in/jin-zhang-439ab51a3/",
                       "LinkedIn Profile"
                     )
                   )
               )
           )
    ),
    #About Daniel
    column(2,
           div(class="panel panel-default", 
               div(class="panel-body",  width = "1000px",
                   align = "center",
                   div(
                     tags$img(src = "daniel.jpg", 
                              width = "50px", height = "50px")
                   ),
                   div(
                     tags$h6("Kaixuan Jiang"),
                     tags$h6( tags$i("UW Student"))
                   ),
                   div(
                     "Aspiring to become a data scientist"
                   ),
                   div(
                     a(
                       # where to link
                       href = "https://www.linkedin.com/in/kaixuan-jiang-1ba0341a4/",
                       # what text to show
                       "LinkedIn Profile"
                     )
                   )
               )
           )
    )
  ),column(3),
  fluidRow(style = "height:150px;")
)

#Tab for the Sources used in this project
sources_panel <- tabPanel(
  title = "Sources",
  h2("Our Sources", align = "center"),
  
  #Layout  using the 12grid system in fluidRow
  fluidRow(
    column(2),
    column(8,
    h3("Drug Overdose Dataset"),
    p("The Opioid Overdose Deaths data contains the total number of deaths from drug overdose in each state from 1999-2018 with the type of drug that caused it. The data has been collected by the National Center for Health Statistics (NCHS) at the Centers for Disease Control and Prevention (CDC), a government organization. Mortality information is collected by state registries and provided to the National Vital Statistics System. Since this is a very big dataset you can't download the complete data, you have to filter out the cause of death which were the indicators (X40,41,43,44,60,61,63,64,85) and (Y10,11,13,14) and then download it. ",
      a(href = "https://wonder.cdc.gov/mcd-icd10.html",
      "Click"
        ), "to view the dataset."),
    h3("Medicaid Spending Dataset"),
    p("The Medicaid data contains the spending on different services like medical products, treatment, and more provided under the Medicaid Program for each and United States as a whole from 1991-2014.The data is collected by the Centers for Disease Control and Prevention (CDC). The dataset contains a lot of different files, we are using the", strong("MEDICAID_AGGREGATE14"), "file.",
      a(href = "http://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/NationalHealthExpendData/Downloads/resident-state-estimates.zip",
        "Click"
      ), "to view the dataset.")
    ),
    column(2)
  )
)


#Combining all the tabs into a navbarPage
ui<- navbarPage(
  title = "",
  home_panel,
  drug_death_count_panel,
  opioid_death_panel,
  drug_medicaid_panel,
  about_panel,
  sources_panel,
  theme = "style.css"
)


