# Loading libraries
library("dplyr")
library("ggplot2")
library("tidyr")
library("maps")
library("mapproj")
library("geojsonio")

# Loading the two dataframes
opioid_df <- read.csv("Multiple Cause of Death, 1999-2018.csv", stringsAsFactors = F)
health_df <- read.csv("MEDICAID_AGGREGATE14.csv", stringsAsFactors = F)

#Sample Opioid Dataframe of top 10 row
sample_opioid_df <- opioid_df %>% 
  select(State, Year, Multiple.Cause.of.death, Multiple.Cause.of.death.Code, Deaths, Population) %>% 
  head(10)

#Sample Medicaid Spending Dataframe of top 10 row
sample_health_df <- health_df %>% 
  select(Region_Name, State_Name, Y2011, Y2012, Y2013, Y2014) %>% 
  head(10)

#filter the top 5 total deaths by drugs states in 2018
Top_5_deaths_2018 <- opioid_df %>%
  filter(Year == "2018") %>%
  select(State, Deaths) %>%
  group_by(State) %>%
  summarize(total_deaths = sum(Deaths)) %>%
  arrange(-total_deaths) %>%
  top_n(5) %>%
  select(State, total_deaths)

#Get the value increased in total death from 1999 to 2018 (in decimal)
increased_df <- opioid_df %>%
  group_by(Year) %>%
  summarize(total_deaths = sum(Deaths)) %>% 
  filter(Year == "1999" | Year == "2018") %>%
  mutate(change = total_deaths[[2]]-total_deaths[[1]]) %>%
  summarize(percent_change = change[[1]]/total_deaths[[2]])

decimal_increased <- increased_df[[1]]

# This function turns the exact proportion(decimal) into a percentage(rounded).
format <- function(decimal) {
  percentage <- paste0(round(decimal * 100), "%")
}

# Get the percentage increased in total death from (1999-2018)
percentage_increased <- format(decimal_increased)
  
#summarize the total deaths
opioid_df_long <- opioid_df %>%
  select(Year, Multiple.Cause.of.death, Deaths) %>% #select relevant data columns
  group_by(Multiple.Cause.of.death, Year) %>% #group by Multiple.Cause.of.death and Year
  summarize(total_deaths = sum(Deaths)) #adding all the states' data together

#spread data in wide version in order to add Multiple.Cause.of.death into three categories
opioid_df_wide <- spread(
  opioid_df_long,
  key = Multiple.Cause.of.death,
  value = total_deaths
)

#set NA value in the data to be 0
opioid_df_wide[is.na(opioid_df_wide)] <- 0

#generate the three categories of drugs and select those three categories and the Year column
q1_plot_df <- opioid_df_wide %>%
  mutate(Heroin = Heroin, illegal_drugs = `Other and unspecified narcotics`+ `Other synthetic narcotics` + Heroin, 
         legal_drugs = Methadone + `Other opioids`) %>%
  select(Year, Heroin, illegal_drugs, legal_drugs)

#gather the data into long version for plotting
q1_plot_df_long <- gather(
  q1_plot_df,
  key = drug_categories,
  value = total_deaths,
  -Year #order by year
)

#plot the line graph
q1_plot <- (ggplot(data = q1_plot_df_long) +
                     geom_point(mapping = aes(x = Year, y = total_deaths, color = drug_categories)) +
                     geom_line(mapping = aes(x = Year, y = total_deaths, color = drug_categories))) +
          labs(
            title = "US drugs deaths over time",
            x = "Year",
            y = "total deaths"
          ) +
            scale_colour_discrete(name = "drug categories", label = c("Heroin", "Illegal drugs", "legal drugs"))

# A function takes drug_category(e.g."legal_drugs") as input and returns the correlation between
# the total deaths of that category each year and the time from 1999 to 2018.
get_correlation <- function(drug_category) {
  df <- q1_plot_df_long %>%
    filter(drug_categories == drug_category)
    correlation <- cor(df$Year, df$total_deaths)
    return(correlation)
}

# making correlation total death from each drug category  
drug_categories <- c("Heroin", "legal_drugs", "illegal_drugs")
corr_time <- c(get_correlation(drug_categories[[1]]),get_correlation(drug_categories[[2]]),get_correlation(drug_categories[[3]]))
corr_df <- data.frame(drug_categories, corr_time)

# merge the correlation dataset with the previous plot dataset, to get a result df
result_df <- left_join(q1_plot_df_long, corr_df, by = "drug_categories")
sample_result_df <- result_df %>% 
  filter(Year == 2018)



# Filtering the dataset for further summary of question 2
opioid_df_map <- opioid_df %>%
  group_by(State) %>%
  summarize("Total Death" = sum(Deaths, na.rm = T))

# Break the dataset to several levels (for question 2)
opioid_df_map$change_label <- cut(opioid_df_map$`Total Death`, breaks = c(0,100,500,1000,5000,10000,15000,20000), labels = c("0-100","100-500","500-1000","1000-5000","5000-10000","10000-15000","15000-20000"))

# Change the states name to lower case so that they can be identified (for question 2)
opioid_df_map$State <- tolower(opioid_df_map$State)

# Calculate the numbers and get the state names that we need for summary (for question 2)
total_death_2018 <- opioid_df %>%
  filter(Year == 2018)
total_death_2018  <- sum(total_death_2018$Deaths, na.rm = T)
death_mean <- mean(opioid_df_map$`Total Death`)
death_mean <- round(death_mean, digits = 0)
death_max <- max(opioid_df_map$`Total Death`)
death_min <- min(opioid_df_map$`Total Death`)
max_state <- opioid_df_map$State[opioid_df_map$`Total Death` == death_max]
min_state <- opioid_df_map$State[opioid_df_map$`Total Death` == death_min]

# Create the table to show the top 6 states that have most death caused by opioid (for question 2)
opioid_df_summary <- opioid_df %>%
  group_by(State) %>%
  summarize("Total Death" = sum(Deaths, na.rm = T), "State Population" = sum(Population, na.rm = T))

opioid_df_summary <- opioid_df_summary[order(-opioid_df_summary$`Total Death`),]
opioid_df_summary <- opioid_df_summary[1:6,]

# Generating the map graphic by using ggplot2 for question 2
states_map <- map_data("state")
opioid_map <- ggplot()
opioid_map <- opioid_map + geom_map(data = states_map, map = states_map,
                                    aes(x = long, y = lat, map_id = region),
                                    fill = "#ffffff", color = "#000000")
opioid_map <- opioid_map + geom_map(data = opioid_df_map, map = states_map,
                                    aes(fill = change_label, map_id = State),
                                    color="#000000")
opioid_map <- opioid_map + scale_fill_brewer(palette = "Reds")
opioid_map <- opioid_map + labs(x = NULL, y = NULL)
opioid_map <- opioid_map + theme(axis.ticks = element_blank()) +
  theme(axis.text = element_blank()) + 
  theme(panel.border = element_blank()) +
  theme(panel.background = element_blank()) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  labs(title = "Total Death Caused by Opioid Overdose in Each State From 1999-2018")




#State that received the most Medicaid 
state_medicaid_max_2014 <-health_df %>% 
  filter(State_Name != "") %>% 
  filter(Y2014 == max(Y2014, na.rm = T)) %>% 
  pull(State_Name)

#Creating a dataframe of the year and total death count
drug_df <- opioid_df %>% 
  select(Year,Deaths,Population) %>%
  filter(Year <2015) %>% 
  group_by(Year) %>% 
  summarise(Total_death = sum(Deaths, na.rm =T))

#Selecting the row we need from Medicaid Data
health_df <- health_df %>% 
  filter(Region_Name == "United States", Item =="Medicaid/Personal Health Care (Millions of Dollars)")

#Droping the columns that we do not need
drops <- c("Code","Group","Region_Number","State_Name","Average_Annual_Percent_Growth","Item","Y1991","Y1992","Y1993","Y1994","Y1995","Y1996","Y1997","Y1998")
medicaid_df <- health_df[ , !(names(health_df) %in% drops)]

#Renaming the columns
colnames(medicaid_df) <- c("State", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009","2010","2011","2012","2013","2014")

#Gathering the data in long format for year and Total spending
medicaid_df <-  medicaid_df %>% 
  gather(key = "Year", value = "spending", -State) %>%
  mutate(Year= as.numeric(Year)) %>% 
  select(Year,spending)

#Joining the opioid and medicaid  dataframes for futher analysis
drug_medi_df <- left_join(drug_df,medicaid_df , by ="Year") %>%
  mutate(spending = spending/100)

#Making the dataframe into long format
drug_medi_df_long <- drug_medi_df%>% 
  gather(key = Type,value = value,-Year)

#Some statistics from the joined dataframe
medicaid_spending_2014 <- medicaid_df[medicaid_df$Year == "2014", "spending"]
Perecent_medicaid <- (medicaid_spending_2014 - medicaid_df[medicaid_df$Year == "1999", "spending"])*100/medicaid_spending_2014
death_cor <- cor(drug_df$Year,drug_df$Total_death)
medicaid_cor <- cor(medicaid_df$Year, medicaid_df$spending)
death_medi_cor <- cor(drug_df$Total_death,medicaid_df$spending)

#Plotting the joined dataframe
plot_q3 <-ggplot(data = drug_medi_df_long, mapping = aes(x = Year, y = value, color = Type) ) +
  geom_point(aes()) +
  geom_line()+
  labs( title = "Medicaid Spending by US Government and Total Drug Overdose death(1999-2014)", x = "Year",y = " ") +
  scale_color_discrete(name = "Type", label = c("Medicaid Spending(in hundred million $US)", "Total Death Count"))+
  scale_x_continuous(name="Year", limits=c(1999, 2014))+
  theme(legend.position = c(0.25, 0.85))
