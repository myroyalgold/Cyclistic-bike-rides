#Prepare

#installation of packages
install.packages("tidyverse")
install.packages("janitor")
install.packages("lubridate")
install.packages("ggplot2")

#loading the packages
library(tidyverse)
library(janitor)
library(lubridate)
library(dplyr)
library(ggplot2)
library(scales)

#I imported the dataset and renamed them as follows(cyclistdata = cd)
cd_1 <- read.csv("202304-divvy-tripdata.csv")
cd_2 <- read.csv("202305-divvy-tripdata.csv")
cd_3 <- read.csv("202306-divvy-tripdata.csv")
cd_4 <- read.csv("202307-divvy-tripdata.csv")
cd_5 <- read.csv("202308-divvy-tripdata.csv")
cd_6 <- read.csv("202309-divvy-tripdata.csv")
cd_7 <- read.csv("202310-divvy-tripdata.csv")
cd_8 <- read.csv("202311-divvy-tripdata.csv")
cd_9 <- read.csv("202312-divvy-tripdata.csv")
cd_10 <- read.csv("202401-divvy-tripdata.csv")
cd_11 <- read.csv("202402-divvy-tripdata.csv")
cd_12 <- read.csv("202403-divvy-tripdata.csv")

#merging data in to a single data set called bike_rides
bike_rides <- rbind(cd_1,cd_2,cd_3,cd_4,cd_5,cd_6,cd_7,cd_8,cd_9,cd_10,cd_11,cd_12)
head(bike_rides)

#checking the structure of the dataset
dim(bike_rides)
glimpse(bike_rides)

#Process: checking and removing empty columns and rows using janitor
bike_rides <- remove_empty(bike_rides, which = c('cols'))
bike_rides <- remove_empty(bike_rides, which = c('rows'))
dim(bike_rides)

#converting started at and ended at to datetime
bike_rides$started_at  <- as_datetime(bike_rides$started_at)
bike_rides$ended_at <- as_datetime(bike_rides$ended_at)

#getting the start and end time in date (ymd)
bike_rides$start_date <- date(bike_rides$started_at)
bike_rides$end_date <- date(bike_rides$ended_at)

#getting the start time and end time in hour
bike_rides$start_hour <- hour(bike_rides$started_at)
bike_rides$end_hour <- hour(bike_rides$ended_at)


#getting the ride length in hour
bike_rides$length_hour <- difftime(bike_rides$ended_at,bike_rides$started_at,units = c("hour"))
bike_rides$length_hour <- as.numeric(bike_rides$length_hour)

#getting the ride length in minutes
bike_rides$length_min <- difftime(bike_rides$ended_at, bike_rides$started_at, units = c("mins"))
bike_rides$length_min <- as.numeric(bike_rides$length_min)

#remove rows that have trip equal to zero and less than 0
bike_rides <- bike_rides[!(bike_rides$length_hour== 0),]
dim(bike_rides)

#getting days of the week from the start and end date
bike_rides$start_day <- wday(bike_rides$start_date, label=TRUE, abbr = FALSE)
bike_rides$end_day <- wday(bike_rides$end_date, label=TRUE, abbr = FALSE)


#to remove unnecessary columns such as ride id, start and end station name, start and end station id, start and end latitude and start and end longitude
bike_rides <- select(bike_rides, -start_station_name,-start_station_id,
                     -end_station_name,end_station_id,-start_lat,-start_lng,
                     -end_lat,-end_lng)
#
dim(bike_rides)
glimpse(bike_rides)
colnames(bike_rides)

#to get minimum and maximum date
mindate <- min(bike_rides$start_date)
maxdate <- max(bike_rides$start_date)




#visualization
#Member types
Member_Type <- ggplot(data = bike_rides) +
  geom_bar(mapping = aes(x=member_casual), fill = "green") +
  scale_y_continuous(labels = label_number()) +  # Format y-axis labels
  theme_minimal() +
  labs(title = "Cyclistic Member Types",
       subtitle = "This shows that most of the clients are members",
       caption = paste0("Data from: ", mindate, " to ", maxdate),
       x = "Member types",
       y = "Number of members") 
Member_Type



# bike used by different riders
Rideable_Type <- ggplot(data = bike_rides) +
  geom_bar(mapping = aes(x=rideable_type), fill = "green") +
  scale_y_continuous(labels = label_number()) +
  facet_wrap(~member_casual)+
  labs(title = "Bike used by different riders",
       subtitle = "Member uses classic and electric bike most and none of the members uses docked bike while some casual riders uses docked bike.",
       caption = paste0("Data from: ", mindate, " to ", maxdate),
       x = "Bike rides",
       y = "Total number of memebers")
Rideable_Type


#bike rides count by hour
bikerides_count_byhour <- bike_rides %>% 
  count(start_hour, sort = TRUE) %>%
  ggplot() + 
  geom_line(aes(x = start_hour, y = n)) +
  scale_y_continuous(labels = label_number()) +
  labs(title = "Count of Bike rides by hour: Previous 12 months",
       y = "Count of rides",
       x = "Start of rides")
bikerides_count_byhour



#calculate average ride length of member type in minutes
#Group by member_casual and calculate the average ride length in minutes
average_ride_length <- bike_rides %>%
  group_by(member_casual) %>%
  summarize(average_length = mean(length_min))

# Plot the graph
ggplot(average_ride_length, aes(x = member_casual, y = average_length)) +
  geom_bar(stat = "identity", fill = "green") +
  labs(title = "Average Ride Length by Member Type",
       subtitle = "The average ride time of Casual Clients is more compared to the Member Clients.",
       x = "Member Type",
       y = "Average Ride Length (minutes)") +
  theme_minimal()

#Rider distribution by start day
rider_day <- ggplot(bike_rides, aes(x = start_day)) +
  geom_bar(fill = "green") +
  facet_wrap(~member_casual) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(labels = label_number()) +
  labs( title = "Rider Distribution by Start Day",
        subtitle = "This shows that Casual Clients use the Company’s service mostly on Saturdays, whereas member clients predominantly engage with the service on weekdays, specifically on Thursday, Wednesday, and Tuesday",
    x = "Start Day",
    y = "Number of Riders")
rider_day



# Trip commencement day distribution
table_data <- bike_rides %>%
  select(start_day, rideable_type, member_casual) 

# Create a ggplot with text labels
table_plot <- ggplot(table_data, aes(x = 1, y = seq_along(start_day))) +
  geom_text(aes(label = paste("Start Day:", start_day, "\nRideable Type:", rideable_type, "\nMember Casual:", member_casual)),
            hjust = 0, vjust = 1, size = 3) +
  labs(title = "Table of Bike Rides Data",
       x = "", y = "") +  # No axis labels
  theme_void()
print(table_plot)



#
