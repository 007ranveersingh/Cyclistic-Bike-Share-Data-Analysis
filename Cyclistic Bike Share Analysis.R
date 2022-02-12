# Cyclistic Bike Share Analysis -> (Feb,2021-Jan,2022)
# This analysis is a part of Google Data Analytics Professional Certificate 
#The purpose of this analysis is to store the downloaded data in single data frame and the perform simple analysis to answer the question: 
# How do members and casual riders use Cyclistic bikes differently?


# Load the packages
library(tidyverse) # for importing data and wrangling data
library(lubridate) # for date functions: helps wrangle date attributes
library(ggplot2) # for visualization
library(scales)

getwd() # displays the working directory
setwd("\\Users\\RANVEER SINGH\\Desktop\\Cyclistic Bike Share Analysis")  # sets your working directory to "\\Users\\RANVEER SINGH\\Desktop\\Cyclistic Bike Share Analysis\\CSV FILES"  

#******************************
#STEP 1: COLLECTING DATA
#******************************
# Upload datasets here
q1_2021_02 <- read_csv("feb_2021.csv")
q1_2021_03 <- read_csv("march_2021.csv")
q2_2021_04 <- read_csv("april_2021.csv")
q2_2021_05 <- read_csv("may_2021.csv")
q2_2021_06 <- read_csv("june_2021.csv")
q3_2021_07 <- read_csv("july_2021.csv")
q3_2021_08 <- read_csv("aug_2021.csv")
q3_2021_09 <- read_csv("sept_2021.csv")
q4_2021_10 <- read_csv("oct_2021.csv")
q4_2021_11 <- read_csv("nov_2021.csv")
q4_2021_12 <- read_csv("dec_2021.csv")
q1_2022_01 <- read_csv("jan_2022.csv")


#**********************************************
#WRANGLE DATA AND COMBINE INTO A SINGLE FILE
#**********************************************
#Check whether each file has same column names
colnames(q1_2021_02)
colnames(q1_2021_03)
colnames(q2_2021_04)
colnames(q2_2021_05)
colnames(q2_2021_06)
colnames(q3_2021_07)
colnames(q3_2021_08)
colnames(q3_2021_09)
colnames(q4_2021_10)
colnames(q4_2021_11)
colnames(q4_2021_12)
colnames(q1_2022_01)

# Inspect the data frames for inconsistencies
str(q1_2021_02)
str(q1_2021_03)
str(q2_2021_04)
str(q2_2021_05)
str(q2_2021_06)
str(q3_2021_07)
str(q3_2021_08)
str(q3_2021_09)
str(q4_2021_10)
str(q4_2021_11)
str(q4_2021_12)
str(q1_2022_01)

# Convert ride_id to character type so that it can stack easily
q1_2021_02 <- mutate(q1_2021_02, ride_id = as.character(ride_id))
q1_2021_03 <- mutate(q1_2021_02, ride_id = as.character(ride_id))
q2_2021_04 <- mutate(q1_2021_02, ride_id = as.character(ride_id))
q2_2021_05 <- mutate(q1_2021_02, ride_id = as.character(ride_id))
q2_2021_06 <- mutate(q1_2021_02, ride_id = as.character(ride_id))
q3_2021_07 <- mutate(q1_2021_02, ride_id = as.character(ride_id))
q3_2021_08 <- mutate(q1_2021_02, ride_id = as.character(ride_id))
q3_2021_09 <- mutate(q1_2021_02, ride_id = as.character(ride_id))
q4_2021_10 <- mutate(q1_2021_02, ride_id = as.character(ride_id))
q4_2021_11 <- mutate(q1_2021_02, ride_id = as.character(ride_id))
q4_2021_12 <- mutate(q1_2021_02, ride_id = as.character(ride_id))
q1_2022_01 <- mutate(q1_2021_02, ride_id = as.character(ride_id))

# Stack individual month's data into one big data frame
all_trips <- bind_rows(q1_2021_02, q1_2021_03, q2_2021_04, q2_2021_05, q2_2021_06, q3_2021_07, q3_2021_08, q3_2021_09, q4_2021_10, q4_2021_11, q4_2021_12, q1_2022_01)
View(all_trips)

# Remove fields "start_lat", "start_lng", "end_lat", "end_lng" as this data isn't needed
all_trips <- all_trips %>%
  select(-c(start_lat, start_lng, end_lat, end_lng))


#*********************************************
#CLEANING AND PREPARING DATE FOR ANALYSIS
#*********************************************
#Inspect the new table "all_trips" that has been created
colnames(all_trips) # List of column names 
nrow(all_trips) # How many rows are in data frame 
dim(all_trips) # Dimensions of the data frame
head(all_trips) # See the first six rows of data frame 
tail(all_trips) # See the last six rows of data frame 
str(all_trips) # See the list of columns and data types
summary(all_trips) # Statistical summary of data 


#There are a few inconsistencies in the data that we need to fix in order to make it fit for analysis
#1. The data can only be aggregated at granular level, we need to add additional columns of data: "day", "month", "year", so that we have more opprotunities to aggregate data
#2. We need to add a calculated field for length of ride "ride_length" for further analytical purposes
#3. There are some entries which are empty, and some where the ride_length shows negative, we need to eliminate those rows

# In the member_casual column we need to replace "Subscriber" with "member" and "Customer" with "casual" 
table(all_trips$member_casual) # Check how many observations fall under each category
# Reassign to the desired values 
all_trips <- all_trips %>%
  mutate(member_casual = recode(member_casual
                                , "Subscriber"= "member"
                                , "Customer"= "casual"
  ))

table(all_trips$member_casual) # Check if the proper number of observations are reassigned

# Add columns that list date, month, and year in which the ride service was used 
all_trips$date <- as.Date(all_trips$started_at) # Creating a new column "date" which shows date in format "yyyy-mm--dd"
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

# Creating a column "ride_length" in all_trips (in seconds)
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

#Inspect the structure of the columns of all_trips
str(all_trips)

# Converting "ride_length" from difftime to numeric so that it's compatible to run calculations on the data
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
# Converting "started_at", "ended_at", "date" into character 
all_trips$started_at <- as.character(all_trips$started_at)
all_trips$ended_at <- as.character(all_trips$ended_at)
all_trips$date <- as.character(all_trips$date)

# Copying data in 
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]
View(all_trips_v2)

# Eliminating rows which contain NA entries in "ride_length"
all_trips_v2 <- all_trips_v2[!is.na(all_trips_v2$ride_length),] 

#****************************************
# STEP 4: CONDUCT DESCRIPTIVE ANALYSIS
#****************************************
# Descriptive analysis on "ride_length" (all calculations in seconds)
summary(all_trips_v2$ride_length)
mean(all_trips_v2$ride_length) # Average length of the ride (total ride length/no. of rides)
median(all_trips_v2$ride_length) # Midpoint number when ride lengths are sorted in ascending way
max(all_trips_v2$ride_length) # Longest ride
min(all_trips_v2$ride_length) # Shortest ride

# Comparing members and casual riders
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

# Comparing the average ride time of member vs casual on each day of the week
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
# In the above result the name of the days of week are out of order, therefore placing them in order
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))


# Analyzing ridership data by type and weekday
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n() 
            ,average_duration = mean(ride_length)) %>% 		
  arrange(member_casual, weekday)		


# Analyzing ridership data by type and month
all_trips_v2 %>% 
  mutate(month = month(started_at, label = TRUE)) %>%
  group_by(member_casual, month) %>%
  summarise(number_of_rides = n() 
            ,average_duration = mean(ride_length)) %>% 		
  arrange(member_casual, month)		




# Visualizing the number of rider by rider type on each day of the week
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

# Visualizing the number of rider by rider type in each month
all_trips_v2 %>% 
  mutate(month = month(started_at, label = TRUE)) %>% 
  group_by(member_casual, month) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, month)  %>% 
  ggplot(aes(x = month, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")



# Visualizing the average duration of bike ride by rider type on each day of the week
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")


# Visualizing the average duration of bike ride by rider type in each month
all_trips_v2 %>% 
  mutate(month = month(started_at, label = TRUE)) %>% 
  group_by(member_casual, month) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, month)  %>% 
  ggplot(aes(x = month, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")


# Create some csv files that we will visualize in tableau
counts_1 <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
write.csv(counts_1, file = '\\Users\\RANVEER SINGH\\Desktop\\Cyclistic Bike Share Analysis\\avg_ride_length.csv')

counts_2 <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$month, FUN = mean)
write.csv(counts_2, file = '\\Users\\RANVEER SINGH\\Desktop\\Cyclistic Bike Share Analysis\\avg_ride_length_each_month.csv')
