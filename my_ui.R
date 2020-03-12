library("shinyWidgets")

#Creating Home Tab
home_panel <- tabPanel(
  title = "Home",
  #Header
  h1("Study on the Opioid Crisis and Medicaid Spending in the United States"),
  #Introduction
  p("Our project will be analyzing data on opioid crisis and Medicaid spending in the United States. Opioids are drugs primarily used in the medical field as a pain reliever, treat cancer patients, anesthesia, and more. There are different types of opioid drugs that are classified as legal or illegal in the United States. Doctors have to prescribe legal drugs like oxycodone, hydrocodone, morphine, and others. Illegal drugs include heroin, synthetic opioids - fentanyl, and more. All these drugs are highly potent, which means even a small amount of the drug can lead to death if not supervized by medical experts. In the 1990s pharmaceutical companies began falsely advertising legal opioid drugs as non-addictive and bribed doctors to often prescribe their patients these drugs. People got hooked to these drugs and often overdosed, which led to an increase in deaths. In 2011, the US government sued pharmaceutical companies and prosecuted doctors which led to leass availability of legal opioids. People who were addicted to these drugs switched to more dangerous illegal drugs like Heroin. The number of deaths from opioid drug overdose has been increasing and this has been termed as the opioid crisis. US government has been spending billions of dollar on Medicaid, law enforcement, and more to fight the crisis. Medicaid is a fedreal and state program that allocates financial help to people towards medical cost like medical products, treatment, and more."),
  p("This analysis would give an insight to the policymakers and the concerned people on how the crisis has worsened over time. It will also shed light on the current situation, and show the impact of Medicaid spending, while also examining if there is a need for other stronger drug policies to deal with the crisis.")
)

#widget slider input to select the range of years
sidebar_content <- sidebarPanel(
  sliderInput(inputId = "year", label = "Year", min = 1999, max = 2014, value = c(1999, 2014), sep = "")
)

#Layout for Question 3
main_content3 <- mainPanel(
  tabsetPanel(
    #Rendering the Plot for Question3
    tabPanel("Plot", plotOutput("plot3")), 
    
    #Rendering the Table for Question3
    tabPanel("Table", tableOutput("table3"), setBackgroundColor(color = 'white'))
  )
)

#Tab for Question3
drug_medicaid_panel <- tabPanel(
  title = "Drug Overdose and Medicaid Spending",
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
  p("In order to analyze the opioid data we selected the varibles Year and Deaths. Consequntly, to sum up all the deaths of each year we used group by Year and summarize function. Futhermore, we also analysed the dataframe containing the Medicaid spending, we filtered the data by Region_Name to be United States and Item to be Medicaid/Personal Health Care (Millions of Dollars) and selected the necessary years. After performing data wranling, we combined the two dataframes to answer the question."),
  h4("Analysis:"),
  p("We correlated overdose death with time, medicaid spending with time, and overdose death  with medicaid spending. Having the correlation value of overdose death compared to time as", strong(death_cor), "we consider it to be linearly growing. We also dervied a strong positive correlation between medicaid spending and time to be", strong(medicaid_cor),"."),
  p("We intially reckoned that if there is a strong positive correlation between overdose deaths and medicaid spending, then medicaid spending is effecient in reducing the overdose deaths. The relationship should be negative or weak(between 0.5 to -0.5) to show that the number of deaths are reducing with more medicaid spending. To our suprise, we found that oue intial belief held. There was a strong positive relationship of", strong(death_medi_cor), "which confirms that the medicaid spending is not helping with the drug crisis."),
  p("Whilst medicaid spending is benefical to a lot of Americans, however it has very little to no effect on drug addicts. Since, there is only a fraction of medicaid spending allocated to drug addicts,the US government should reconsider to invest this money in other programs and policies. One can notice how the action taken by the US government against pharmaceutical companies and changes made in drug policies help reduce the total overdose deaths between 2011-2012. The government can replicate similar drug policies and actions, while enforcing strong laws against illegal drugs.")
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
  drug_medicaid_panel,
  about_panel,
  sources_panel,
  theme = "style.css"
)

