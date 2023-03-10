options(scipen = 999)
# Packages ----------------------------------------------------------------

library(dplyr)
library(tidyverse)

# Creating DataSets -------------------------------------------------------

feb2022 = read_csv("/202202-divvy-tripdata.csv")
mar2022 = read.csv("/202203-divvy-tripdata.csv")
apr2022 = read.csv("/202204-divvy-tripdata.csv")
may2022 = read.csv("/202205-divvy-tripdata.csv")
jun2022 = read.csv("/202206-divvy-tripdata.csv")
jul2022 = read.csv("/202207-divvy-tripdata.csv")
aug2022 = read.csv("/202208-divvy-tripdata.csv")
sep2022 = read.csv("/202209-divvy-publictripdata.csv")
oct2022 = read.csv("/202210-divvy-tripdata.csv")
nov2022 = read.csv("/202211-divvy-tripdata.csv")
dec2022 = read.csv("/202212-divvy-tripdata.csv")
jan2023 = read.csv("/202301-divvy-tripdata.csv")

# Most Data: June, July August, September
# Least Data: January 2023, February, March, December

# Test Columns ------------------------------------------------------------
# Test to see if columns match in classes. If not, check how they don't match
# It is found that jan2023 and feb2022 do not match datatypes

sapply(feb2022, class) == sapply(jan2023, class)

view(sapply(feb2022, class))

view(sapply(jan2023, class))

# Convert Columns ---------------------------------------------------------

feb2022$started_at = as.character(feb2022$started_at)
feb2022$ended_at = as.character(feb2022$ended_at)

## test to see if it binds correctly. If it does, we remove it afterwards.
test1 = bind_rows(feb2022, jan2023)
rm(test1)

# Binding Process ---------------------------------------------------------

annual_2022 = bind_rows(feb2022, mar2022, apr2022, may2022, jun2022, 
                        jul2022, aug2022, sep2022, oct2022, nov2022, dec2022, jan2023)

# Remove Excess Data for the sake of storing the data
rm(feb2022,mar2022,apr2022,may2022,jun2022,jul2022,aug2022,sep2022,oct2022,
   nov2022,dec2022,jan2023)

# Further cleaning --------------------------------------------------------
# Want to calculate ride_length in minutes and then round it to 1 decimal

annual_2022$ride_length = round(difftime(annual_2022$ended_at, 
                                         annual_2022$started_at, units="mins"), 1)

# Checking the value of $ride_length
View(slice_max(annual_2022, annual_2022$ride_length))
View(slice_min(annual_2022, annual_2022$ride_length))
#From here, it is obvious that we are dealing weird values. Some values are too high, 
#which could infer maintenance, while others indicate a negative value, thus we should filter them out

# Cleaning out bad values -------------------------------------------------
## Removing any values that are the following. NA/NULL or blank, The end station being the repair shop
## Any value that is negative, or exceeds 3 hours.

annual_2022_v2 = annual_2022 %>% 
  filter(end_station_name != "Base - 2132 W Hubbard Warehouse") %>% 
  filter(!is.na(start_station_name) | start_station_name != "") %>% 
  filter(!is.na(start_station_id) | start_station_id != "") %>% 
  filter(!is.na(end_station_name) | end_station_name != "") %>% 
  filter(!is.na(end_station_id) | end_station_id != "") %>% 
  filter(between(as.double(ride_length), 0, 180))

## We can start removing the unfiltered data
rm(annual_2022)

## Next I want to define the time of month and day by running these functions.
## This will allow me to later on compare time and month of the year.

annual_2022_v2 = annual_2022_v2 %>% 
  mutate(start_hour = hour(started_at), day_of_week = weekdays(as.Date(started_at)), 
         month_of_day = month(as.Date(started_at)))

## Now will define time of day using case_when

annual_2022_v2 = annual_2022_v2 %>% 
  mutate(time_of_day = case_when(between(start_hour, 4, 9) ~ "Morning", 
                                 between(start_hour, 10, 15 ) ~ "Noon", 
                                 between(start_hour, 16, 21) ~ "Evening",
                                 TRUE ~ "Dusk"))

## Have to reorder them in a correct format and simplify the words
annual_2022_v2$day_of_week <- factor(annual_2022_v2$day_of_week, labels = 
                                       c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"),
                                     levels = c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                                "Friday", "Saturday", "Sunday"))

annual_2022_v2$month_of_day <- factor(annual_2022_v2$month_of_day, labels = 
                                        c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                          "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), 
                                      levels = c("1", "2", "3", "4", "5", "6", "7", "8",
                                                 "9", "10", "11", "12"))

annual_2022_v2$time_of_day <- factor(annual_2022_v2$time_of_day, 
                                     labels = c("Morn", "After", "Even", "Dusk"),
                                     levels = c("Morning", "Noon", "Evening", "Dusk"))

annual_2022_v2$rideable_type <- factor(annual_2022_v2$rideable_type, 
                                     labels = c("Clas", "Dock", "Elec"),
                                     levels = c("classic_bike", "docked_bike"
                                                , "electric_bike"))

# Data Lists -------------------------------------------------------------

## Summary data based on Casual vs Member, Rideable Types
## Returns # of users, mean, median, quartile 25 + 75, std, 95% Conf In of ride_length
basic_data_2022 = annual_2022_v2 %>% 
  group_by(member_casual, rideable_type) %>% 
  filter(rideable_type != "Dock") %>% 
  summarise(users = as.numeric(n()), mean_ride = round(mean(ride_length), 2), 
            median_ride = median(ride_length),
            qua_25 = quantile(ride_length, probs = c(0.25)), 
            qua_75 = quantile(ride_length, probs = c(0.75)),
            st_de = round(sd(ride_length), 2),
            left_95_CONF = round(mean_ride - qt(0.975, df = users-1)*st_de/sqrt(users), 2),
            right_95_CONF = round(mean_ride + qt(0.975, df = users-1)*st_de/sqrt(users), 2)
            )

  
## Data organizes based on month, day, time, rideable_type
## Returns a mean, median, and # of users.

data_2022 = annual_2022_v2 %>% 
  filter(rideable_type != "Dock") %>% 
  group_by(month_of_day, day_of_week, time_of_day, member_casual, rideable_type) %>% 
  summarise(mean_ride = round(mean(ride_length),1), 
            median_ride = median(ride_length), 
            users = as.numeric(n()))

## Want to create a data list of the highest and lowest performing months
lowest_2022 = filter(data_2022, month_of_day %in% c("Feb", "Mar", "Dec", "Jan"))
highest_2022 = filter(data_2022, month_of_day %in% c ("Jun", "Jul", "Aug", "Sep"))

## Used to calculate users in locations

location_2022 = annual_2022_v2 %>% 
  group_by(month_of_day, member_casual, start_station_name) %>% 
  summarize(users = n()) %>% 
  slice_max(n=10, users)

## Used for casual users only

location_2022_c = location_2022 %>% 
  filter(member_casual == "casual")

## Checks total ride time

ride_time_2022 = annual_2022_v2 %>% 
  filter(rideable_type != "Dock") %>% 
  group_by(month_of_day, day_of_week, time_of_day, member_casual, rideable_type) %>% 
  summarize(total = sum(ride_length)) 


## Checks if ride time is below and over 45 minutes or under 31 minutes

ride_over_2022 = annual_2022_v2 %>% 
  filter(rideable_type != "Dock") %>% 
  group_by(month_of_day, rideable_type, member_casual) %>% 
  reframe(under = sum(ride_length < 31),
          over = sum(ride_length > 45))

r_over_2022 = annual_2022_v2 %>% 
  filter(rideable_type != "Dock") %>% 
  group_by(member_casual) %>% 
  reframe(under = sum(ride_length < 31),
          over = sum(ride_length > 45))

# Charts ------------------------------------------------------------------

## Basic Data. Shows number of users given bike type
users_total = basic_data_2022 %>% 
  ggplot(aes(rideable_type, users, fill=member_casual)) + 
  scale_y_continuous(labels = scales::comma) +
  labs(y= "Users", x = "Bike Type") + ggtitle("# of Users") +
  stat_summary(
    geom = "bar", fun = "sum", position = "dodge"
  )

## Number of Users during the time, day, and month -------------------------

###Shows how many members vs casuals on a given day during a certain time

users_day_2022 = data_2022 %>% 
  ggplot(aes(time_of_day, users, fill=member_casual)) + 
  scale_y_continuous(labels = scales::comma) +
  labs(y= "Users", x = "Day") + facet_wrap(~day_of_week) +
  ggtitle("# of Users per day") +
  stat_summary(
    geom = "bar", fun = "sum", position = "dodge"
  )

###Shows how many members vs casuals on a given day per month
users_month_2022 = data_2022 %>% 
  ggplot(aes(day_of_week, users, fill=member_casual)) + 
  scale_y_continuous(labels = scales::comma) +
  labs(y= "Users", x = "Month") + facet_wrap(~month_of_day) + 
  ggtitle("# of Users per month") +
  stat_summary(
    geom = "bar", fun = "sum", position = "dodge"
  )

###Shows how many users ride per bike type on a given day
users_ride_2022 = data_2022 %>% 
  ggplot(aes(factor(day_of_week), users, fill=member_casual)) +
  scale_y_continuous(labels = scales::comma) + facet_wrap(~rideable_type) +
  labs(y= "Users", x = "Day") + ggtitle("# of Users of Ride Type per Day") + 
  stat_summary(
    geom = "bar", fun = "sum", position = "dodge"
  )


## Total Ride Length during time, day, and month ---------------------------

### Shows the total ride time on a given day
total_day_2022 = ride_time_2022 %>% 
  ggplot(aes(factor(time_of_day), total, fill=member_casual)) +
  scale_y_continuous(labels = scales::comma) + facet_wrap(~day_of_week) +
  labs(y= "Time (in mins)", x = "Day") + 
  stat_summary(
    geom = "bar", fun = "sum", position = "dodge"
  )

### Shows the total ride time on a given month
total_month_2022 = ride_time_2022 %>% 
  ggplot(aes(factor(day_of_week), total, fill=member_casual)) +
  scale_y_continuous(labels = scales::comma) + facet_wrap(~month_of_day) +
  labs(y= "Time (in mins)", x = "Day") + 
  ggtitle("Total Ride Length Per Month") +
  stat_summary(
    geom = "bar", fun = "sum", position = "dodge"
  )

### Is the total ride time per bike type on a given day
total_ride_2022 = ride_time_2022 %>% 
  ggplot(aes(factor(day_of_week), total, fill=member_casual)) +
  scale_y_continuous(labels = scales::comma) + facet_wrap(~rideable_type) +
  labs(y= "Time (in mins)", x = "Day") + 
  ggtitle("Total Ride Length of Ride Type Per Day") +
  stat_summary(
    geom = "bar", fun = "sum", position = "dodge"
  )

# Average Ride Time during time, day, and month --------------------------------

### Shows average time riding during on a given day
average_day_2022 = data_2022 %>% 
  ggplot(aes(factor(time_of_day), mean_ride, fill=member_casual)) + 
  geom_bar(stat = "identity", position="dodge") +
  labs(y= "Time (in mins)", x = "Day") + facet_wrap(~day_of_week) + 
  ggtitle("Average Ride Length Per Day")

### Shows average time riding during on a given month
average_month_2022 = data_2022 %>% 
  ggplot(aes(day_of_week, mean_ride, fill=member_casual)) + 
  geom_bar(stat = "identity", position="dodge") +
  labs(y= "Time (in mins)", x = "Day") + facet_wrap(~month_of_day) + 
  ggtitle("Average Ride Length Per Month")

### Shows average time riding per bike type on a given day
average_ride_2022 = data_2022 %>% 
  ggplot(aes(factor(day_of_week), mean_ride, fill=member_casual)) +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_y_continuous(labels = scales::comma) + facet_wrap(~rideable_type) +
  labs(y= "Time (in mins)", x = "Day") + 
  ggtitle("Average Ride Length of Ride Type Per Day")


## Ride_threshold ----------------------------------------------------------

### Looks at how many users ride their bikes over 45 minutes

ride_over_stat_2022 = ride_over_2022 %>% 
  ggplot(aes(factor(rideable_type), over, fill=member_casual)) +
  scale_y_continuous(labels = scales::comma) +
  labs(y= "Users", x = "Ride Type") + 
  ggtitle("# of Users over 45 mins of Ride Type") +
  stat_summary(
    geom = "bar", fun = "sum", position = "dodge"
  )

### Looks at how many users ride their bikes under 31 minutes.

ride_under_stat_2022 = ride_over_2022 %>% 
  ggplot(aes(factor(rideable_type), under, fill=member_casual)) +
  scale_y_continuous(labels = scales::comma) +
  labs(y= "Users", x = "Ride Type") + 
  ggtitle("# of Users 30 mins and under of Ride Type") +
  stat_summary(
    geom = "bar", fun = "sum", position = "dodge"
  )

### Looks at how many users ride their bikes over 45 minutes on a given month

ride_over_month_2022 = ride_over_2022 %>% 
  ggplot(aes(factor(rideable_type), over, fill=member_casual)) +
  scale_y_continuous(labels = scales::comma) + facet_wrap(~month_of_day) +
  labs(y= "Users", x = "Ride Type") + 
  ggtitle("# of Users over 45 mins of Ride Type Per Month") +
  stat_summary(
    geom = "bar", fun = "sum", position = "dodge"
  )

### Looks at how many users ride their bikes under 31 minutes on a given month

ride_under_month_2022 = ride_over_2022 %>% 
  ggplot(aes(factor(rideable_type), under, fill=member_casual)) +
  scale_y_continuous(labels = scales::comma) + facet_wrap(~month_of_day) +
  labs(y= "Users", x = "Ride Type") + 
  ggtitle("# of Users 30 mins and under of Ride Type Per Month") +
  stat_summary(
    geom = "bar", fun = "sum", position = "dodge"
  )


## Location ----------------------------------------------------------------

### Compares Casual vs Member users on location

location_month_2022 = location_2022 %>% 
  ggplot(aes(start_station_name, users, fill=member_casual)) + 
  geom_bar(stat = "identity", position="dodge") +
  labs(y= "Users", x = "Location") + facet_wrap(~month_of_day) + labs(x = NULL) +
  ggtitle("# of Users in a Certain Location")

### Compares Locations for Casual Users

location_month_c_2022 = location_2022_c %>% 
  ggplot(aes(start_station_name, users, fill=member_casual)) + 
  geom_bar(stat = "identity", position="dodge") +
  labs(y= "Users", x = "Location") + facet_wrap(~month_of_day) + labs(x = NULL) +
  ggtitle("# of Casuals in a Certain Location")
