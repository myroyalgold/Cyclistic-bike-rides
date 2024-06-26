# Google Data Analytics Course Capstone Project: 
## Track A
### Case Study 1 Cyclistic Bike Share Analysis

### Problem Statement: How does a bike-share navigate speedy success?
- Disribution by member types.
- Bike type used by different riders.
- Minimum and Maximum ride length.
- Average ride length of member types in minutes.
- Rider distribution by start day.
- count of bike rides per hour.


### Introduction 
As the capstone project for the Google Data Analytics Certificate, I'm tasked with analyzing a public dataset for Cyclistic, an imaginary bike-share company located in Chicago. Throughout this case study, I'll utilize the R programming language for data analysis and visualization.

### About the Company
<br> Cyclistic was launched in 2016 and has since expanded its program, boasting a fleet of 5,824 bicycles meticulously tracked through geotracking technology and securely stationed across a network of 692 locations throughout Chicago.
<br> Cyclistic distinguishes itself by providing a range of options beyond traditional bicycles, including reclining bikes, hand tricycles, and cargo bikes. This inclusive approach aims to cater to individuals with disabilities and those unable to use standard two-wheeled bikes, making bike-sharing accessible to a broader audience.
<br> The majority of riders opt for traditional bikes, but approximately 8% of riders use the assistive options.
<br> Cyclistic users are more likely to ride for leisure, but about 30% use the bikes to commute to work each day.

### Cyclistic Marketing Strategy
<br> Historical perspective/Approach: Cyclistic’s marketing strategy relied on building general awareness and appealing to broad consumer segments.
<br> Pricing plan: One approach that helped make these things possible was the flexibility of its pricing plans: single-ride passes, full-day passes, and annual memberships.
<br> Customer categprization:
-  Casual Riders: Customers who purchase single-ride or full-day passes are referred to as casual riders.
- Cyclistic Members: Customers who purchase annual memberships are Cyclistic members.
<br> Cyclistic’s finance analysts have concluded that annual members are much more profitable
than casual riders.

### Marketing Team
<br> The director of marketing and manager: Lily Moreno is the director of marketing and manager. Moreno is responsible for the development of campaigns and initiatives to promote the bike-share program. These may include email, social media, and other channels
<br> Marketing Analytics Team :Marketing analytics team: A group of data analysts tasked with gathering, examining, and presenting data crucial for shaping Cyclistic's marketing strategy.

### Current Challenge:
<br> Business Focus: The company's future prosperity hinges on maximizing the number of annual memberships.
<br> Marketing Goal: The marketing team seeks to discern the varying usage patterns between casual riders and annual members of Cyclistic bikes in order to craft a fresh marketing strategy. The objective is to convert casual riders into annual members.

# The following data analysis steps will be followed:
Ask, Prepare, Process, Analyze, Share, Act.
## Ask
### Guiding Questions:
<br> What is the problem you are trying to solve?
- The problem to solve is understanding how annual members and casual riders use Cyclistic bikes differently.
<br> How can your insights drive business decisions?
-  The conclusions drawn from the analysis will shape the creation of a fresh marketing strategy aimed at transitioning casual riders into annual members. These data-driven insights will steer decision-making regarding the customization of marketing endeavors to enhance customer attraction and retention.

### The business task:
Understand how annual members and casual riders use Cyclistic bikes differently.

### The key stakeholders are:
- Lily Moreno: Director of Marketing and Manager
- Cyclistic Marketing Analytics Team
- Cyclistic Executive Team

## Prepare
### Data Sources: Link to Cyclistic Data:https://divvy-tripdata.s3.amazonaws.com/index.html

### Dataset Downloaded
- 202304-divvy-tripdata.csv
- 202305-divvy-tripdata.csv
- 202306-divvy-tripdata.csv
- 202307-divvy-tripdata.csv
- 202308-divvy-tripdata.csv
- 202309-divvy-tripdata.csv
- 202310-divvy-tripdata.csv
- 202311-divvy-tripdata.csv
- 202312-divvy-tripdata.csv
- 202401-divvy-tripdata.csv
- 202402-divvy-tripdata.csv
- 202403-divvy-tripdata.csv


### Steps Followed after Downloading the Dataset into my Computer:
I downloaded dataset covering bike rides between April 2023 to March 2024 for the purpose of this analysis.
<br> The data is in CSV format.

#### Preparation of Rstudio for the Analysis

Installation and loading of Required Packages:  
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

# Data Manipulation
- converted started at and ended at to datetime
- getting the start and end time in date (ymd)
- getting the start time and end time in hour
- getting the ride length in hour
- getting the ride length in minutes
- remove rows that have trip equal to zero and less than 0
- getting days of the week from the start and end date
-  removed unused columns such as start and end station name, start and end station id, start and end latitude and start and end longitude as instructed.
-  get minimum and maximum date of rides

# Data visualization
![member_type](https://github.com/myroyalgold/cyclicstic-bike-ride/assets/107118603/6ba69ad3-a895-4fc0-9c46-809493051b9e)

This shows that most of the clients are members.


![average_ride_length](https://github.com/myroyalgold/cyclicstic-bike-ride/assets/107118603/8282d0e7-5759-4293-86cd-8e1cb13676ef)

The average ride time of Casual Clients is more compared to the Member Clients.


![rideable_type](https://github.com/myroyalgold/cyclicstic-bike-ride/assets/107118603/134ac1cd-f75d-46d1-a50e-190b7c0d9583)

Member uses classic and electric bike most and none of the members uses docked bike while some casual riders uses docked bike.


![ridecounts](https://github.com/myroyalgold/cyclicstic-bike-ride/assets/107118603/28b89533-d31c-4a82-b371-9def8d09b19c)


![rider_distribution_byday](https://github.com/myroyalgold/cyclicstic-bike-ride/assets/107118603/3e24ec9a-21e6-4061-9f38-b1fa42b6452f)

This shows that Member Clients use the Company’s service mostly on Thursday, whereas Casual clients engage with the service on Saturday.
Also, Member clients uses the engage with the cyclistic service most.


# Recommendation
To accomplish the objective of converting casual riders into annual members, you can consider the following recommendations:
- Develop targeted marketing campaigns aimed at casual riders, highlighting the benefits of becoming annual members.
- Offer discounts or incentives to encourage casual riders to sign up for annual memberships. This could include discounted membership fees, rewards for signing up.
- Encourage annual members to share their positive experiences with others, creating social proof and word-of-mouth referrals.
- Increase awareness through targeted educational campaigns and social media outreach.

By putting these suggestions into action, you can successfully motivate occasional riders to become annual members, thereby increasing 
