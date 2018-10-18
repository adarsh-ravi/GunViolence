#initialize the libraries

library(plotly)
library(ggplot2)
library(data.table)
library(dplyr)
library(gapminder)
library(gganimate)
library(scales)
library(googleVis)
library(lubridate)
library(highcharter)
library(ggthemes)
library(tidyr)
library(anim.plots)


#Set Working Directory
setwd("H:\\Courses\\Data Visualization\\Projects\\Project1\\Dataset3 Gun Violence")

#Read All Files
accidental_deaths <- read.csv("accidental_deaths.csv")
accidental_deaths_children <- read.csv("accidental_deaths_children.csv")
accidental_deaths_teens <- read.csv("accidental_deaths_teens.csv")
accidental_injuries <- read.csv("accidental_injuries.csv")
accidental_deaths <- read.csv("accidental_deaths.csv")
accidental_injuries_children <- read.csv("accidental_injuries_children.csv")
accidental_injuries_teens <- read.csv("accidental_injuries_teens.csv")
children_injured <- read.csv("children_injured.csv")
children_killed <- read.csv("children_killed.csv")
mass_shootings_2014 <- read.csv("mass_shootings_2014.csv")
mass_shootings_2015 <- read.csv("mass_shootings_2015.csv")
mass_shootings_2016 <- read.csv("mass_shootings_2016.csv")
mass_shootings_all <- read.csv("mass_shootings_all.csv")
officer_involved_shootings <- read.csv("officer_involved_shootings.csv")
teens_injured <- read.csv("teens_injured.csv")
teens_killed <- read.csv("teens_killed.csv")

#Extract Year into a column
mass_shootings_2014$Year <- format(as.Date(mass_shootings_2014$Incident.Date, "%B %d, %Y"), "%Y") 
mass_shootings_2015$Year <- format(as.Date(mass_shootings_2015$Incident.Date, "%B %d, %Y"), "%Y") 
mass_shootings_2016$Year <- format(as.Date(mass_shootings_2016$Incident.Date, "%B %d, %Y"), "%Y") 

#Extract Month into a column
mass_shootings_2014$Month <- format(as.Date(mass_shootings_2014$Incident.Date, "%B %d, %Y"), "%B") 
mass_shootings_2015$Month <- format(as.Date(mass_shootings_2015$Incident.Date, "%B %d, %Y"), "%B") 
mass_shootings_2016$Month <- format(as.Date(mass_shootings_2016$Incident.Date, "%B %d, %Y"), "%B") 

#Bind All data frames for mass shooting from 2014-2016 into a single data frame
mass_shootings <- rbind(mass_shootings_2014, mass_shootings_2015, mass_shootings_2016)

#Get the count of Mass Shooting incidents by State
ms_byState <- as.data.frame(table(mass_shootings$State))
ms_byState

#Plot Bar graph of the # Mass Shootings occured in each state between 2014-2016
ggplot(ms_byState, aes(x=ms_byState$Var1, y=ms_byState$Freq)) + xlab("State") + ylab("Count") + geom_bar(stat = "Identity", fill="indian red", col="black") + theme(axis.text.x = element_text(angle = 90), title = element_text(hjust = 0.5)) + ggtitle("Mass Shootings in each State")

#Get all mass shooting data for California
ms_California <- mass_shootings[mass_shootings$State=="California",]

#Get count of mass shootings in California by year
ms_CaliforniaByYear <- as.data.frame(table(ms_California$Year))

#Plot graph of Mass shootings in california by year
ggplot(ms_CaliforniaByYear, aes(x=ms_CaliforniaByYear$Var1, y=ms_CaliforniaByYear$Freq)) + xlab("Year") + ylab("Count") + geom_bar(stat = "Identity", fill="goldenrod", col="blue") + theme(title = element_text(hjust = 0.5)) + ggtitle("Mass Shootings in California (2014-2016)")

#Get # Mass Shooting incidents in each state for each year from 2014-2016
ms_State_Year <- as.data.frame(table(mass_shootings$State, mass_shootings$Year))
ms_State_Year <- ms_State_Year[order(ms_State_Year$Var1),]
colnames(ms_State_Year)[1] <- "State"
colnames(ms_State_Year)[2] <- "Year"
colnames(ms_State_Year)[3] <- "Count"
head(ms_State_Year)

#Plot stacked bar graph for # Mass Shooting incidents in each state for each year from 2014-2016
ggplot(ms_State_Year, aes(y=Count, x=State, fill=Year)) + geom_bar(stat = "Identity") + theme(axis.text.x = element_text(angle = 90), title = element_text(hjust = 0.5)) + ggtitle("Mass Shootings in each State (2014-2016)") + xlab("State") + ylab("Count")


#Add Day, Month, Year and Weekday columns for the mass shootings for further analysis
mass_shootings$Day <- factor(day(as.POSIXct(mass_shootings$Incident.Date, format = "%B %d, %Y")))
mass_shootings$Month <- factor(month(as.POSIXct(mass_shootings$Incident.Date, format = "%B %d, %Y"), label = TRUE))
mass_shootings$Year <- factor(year(as.POSIXct(mass_shootings$Incident.Date, format = "%B %d, %Y")))
mass_shootings$Weekday <- factor(wday(as.POSIXct(mass_shootings$Incident.Date, format = "%B %d, %Y"), label = TRUE))

#Get sum of people killed in mass shootings by month and year 
ms_month_year <- mass_shootings %>%
  group_by(Year, Month) %>%
  dplyr::summarize(TotalKilled = sum(X..Killed))


#Plot heat map for # people killed in mass shootings by month and year
ggplot(ms_month_year, aes(Month, Year, fill = TotalKilled)) + 
geom_tile(color = "black", size = 0.1) + 
 scale_fill_gradientn(colors = c("chartreuse4","chartreuse3","red2", "red3", "red4")) + 
  theme(legend.position = "top") +
  labs( title = "Mass shooting killed by Year and Month (2014-2016)") +
  geom_text(aes(label=TotalKilled), color='white')


#Get sum of people injured in mass shootings by month and year
ms_month_year_Injured <- mass_shootings %>%
  group_by(Year, Month) %>%
  dplyr::summarize(TotalInjured = sum(X..Injured))

#Plot heat map for # people injured in mass shootings by month and year
ggplot(ms_month_year_Injured, aes(Month, Year, fill = TotalInjured)) + 
  geom_tile(color = "white", size = 0.1) + 
  scale_fill_gradientn(colors = c("chartreuse4","chartreuse3","red2", "red3", "red4")) + 
  theme(legend.position = "top") +
  labs( title = "Mass shooting Injured by Year and Month (2014-2016)") +
  geom_text(aes(label=TotalInjured), color='white')


#Get # people injured and killed during each mass shooting incident
ms_ByDate <- mass_shootings %>% group_by(mass_shootings$Date) %>% dplyr::summarize(TotalInjured = sum(X..Injured), TotalKilled = sum(X..Killed))
colnames(ms_ByDate)[1] <- "Date"

#Plot a line chart depicting the # people injured & killed during all mass shooting incident
plot(gvisLineChart(ms_ByDate, xvar = 'Date', yvar = c("TotalInjured", "TotalKilled"), options(list(title="# Killed and Injured in Mass Shootings (2014 -2016)", titleTextStyle="{color:'red',fontName:'Courier',fontSize:16}", curveType='function'))))


#Get # people killed due to mass shooting in each state
state_killed <- mass_shootings %>% group_by(mass_shootings$State) %>% dplyr::summarize(Killed = sum(X..Killed))
colnames(state_killed)[1] <- "State"

#Plot a geo heat map of # people killed
GeoStates_Killed <- gvisGeoChart(state_killed, "State", "Killed",
                          options=list(region="US", 
                                       displayMode="regions", 
                                       resolution="provinces",
                                       colors="['#548B54', '#7ccd7c', '#90ee90', '#ee0000', '#cd0000', '#8b0000']",
                                       width=800, height=600))
plot(GeoStates_Killed)


#Get # people injured due to mass shooting in each state
state_injured <- mass_shootings %>% group_by(mass_shootings$State) %>% dplyr::summarize(Injured = sum(X..Injured))
colnames(state_injured)[1] <- "State"


#Plot a geo heat map of # people injured
GeoStates_Injured <- gvisGeoChart(state_injured, "State", "Injured",
                                 options=list(region="US", 
                                              displayMode="regions", 
                                              resolution="provinces",
                                              colors="['#548B54', '#7ccd7c', '#90ee90', '#ee0000', '#cd0000', '#8b0000']",
                                              width=1200, height=600))
plot(GeoStates_Injured)

#Create an animated scatter plot of the # people injured/killed in state every year
gg <- ggplot(mass_shootings, aes(x= X..Killed, y= X..Injured, color= State)) + geom_point(aes(size=X..Killed, frame= Year)) + xlab("Killed") + ylab("Injured")
ggplotly(gg)



#Aggregate the # people killed & injured in officer involved shooting  by state and City/County

officer_involved_shootings$Year <- format(as.Date(officer_involved_shootings$Incident.Date, "%B %d, %Y"), "%Y")

ois_state <- officer_involved_shootings %>% group_by(officer_involved_shootings$City.Or.County, officer_involved_shootings$State) %>% dplyr::summarize(Injured = sum(X..Injured), Killed = sum(X..Killed))
head(ois_state)

gg_ois_state <-ggplot(ois_state, aes(y=ois_state$Killed, x=ois_state$Injured, color = ois_state$`officer_involved_shootings$City.Or.County`)) + geom_point(aes(size=Killed, frame = ois_state$`officer_involved_shootings$State`)) + xlab("Injured") + ylab("Killed")
ggplotly(gg_ois_state)
