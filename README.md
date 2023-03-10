# Analysis of Divvy Bikeshare services

**Albert Wang 3/10/2023**

# Introduction

In 2013, Divvy launched a successful bike-share service. Since then, the program has grown to a fleet of 16,500 bicycles that are geotracked and locked into a network of over 800 stations across Chicago. The bikes can be unlocked from one station and returned to any other station in the system anytime. 

[https://chi.streetsblog.org/2021/07/09/divvy-kicks-off-west-side-expansion-with-3500-bikes-107-e-station-parking-areas/](https://chi.streetsblog.org/2021/07/09/divvy-kicks-off-west-side-expansion-with-3500-bikes-107-e-station-parking-areas/)

Until now, Divvy’s marketing strategy relied on building general awareness and appealing to broad consumer segments. One approach that helped make these things possible was the flexibility of its pricing plans: single-ride passes, full-day passes, and annual memberships. Customers who purchase single-ride or full-day passes are referred to as casual riders. Customers who purchase annual memberships are members.

1. Membership = $10 per month plus $119 annual = $20 per month
2. Day pass = $16.50 for 24 hrs on 3 hr limit
3. Single ride = $1, includes 30mins for free, $0.17 per min = 142 mins for $20 ride.

[https://divvybikes.com/pricing](https://divvybikes.com/pricing)

Divvy’s finance analysts have concluded that annual members are much more profitable than casual riders. Although the pricing flexibility helps Divvy attract more customers, it is believed that maximizing the number of annual members will be key to future growth. Rather than creating a marketing campaign that targets all-new customers, Moreno believes there is a very good chance to convert casual riders into members. She notes that casual riders are already aware of the Divvy program and have chosen Divvy for their mobility needs.

### Objectives

Divvy has set a clear goal: Design marketing strategies aimed at converting casual riders into annual members. In order to do that:

1. They need to identify what differs between the Casual and Member riders ie: location, length of ride, what time, and month they ride.
2. What sort of strategies could be implemented to convert these casual riders to become Divvy members?

### Summary of Findings

1. In general, casual riders tend to ride more during the Afternoon and Evening, this is especially prevalent during the weekends.
    1. Throughout the year, the highest amount of casual riders occur during May, June, July, meanwhile the lowest amount occur during December, January, and February.
        1. Note: There are more casual vs members on weekends of June and July
2. The total ride time for casual riders during the weekends is much higher than during the weekdays. ie: Saturday recorded 7.5 million minutes vs 3 million minutes on a Tuesday
    1. Throughout the year, the highest amount of total ride time occur during the weekends of May, June, July, and August.
        1. Note: The only months members rode more than casuals are November, December, January, February. Indicating that despite there being more members, casual riders typically ride longer.
3. The average ride time for casual riders is greater than members , regardless of time, day, and month.
    1. The only months where the average ride time is consistently below 20 mins for casual riders are November, December, and January
        1. Note: There are 25,000 casual riders who rode more than 45 minutes vs 6,800 members.
4. The top three stations for casual riders are Streeter Drive & Grand Avenue, DuSable Lake Shore & Monroe Street, and Millenium Park
    1. The stations that casual riders go to throughout the year are Shedd Aquarium, Streeter Drive & Grand Avenue, and Millenium Park
        1. Note: On the most popular months, it is more concentrated in the North East side of Chicago, meanwhile on the least popular months, it is more spread-out across the strip of Chicago.

# Coding

### Set-up

scipen helps remove scientific values. 

Added libraries and Datasets

```r
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
```

Test to see if columns match in data-type before merging

```r
# Test Columns ------------------------------------------------------------
# Test to see if columns match in classes. If not, check how they don't match
# It is found that jan2023 and feb2022 do not match datatypes
sapply(feb2022, class) == sapply(jan2023, class)
view(sapply(feb2022, class))
view(sapply(jan2023, class))
```

Because it didn’t match, had to convert feb2022$started_at as.char and then the binding was successful.

```r
# Convert Columns ---------------------------------------------------------
feb2022$started_at = as.character(feb2022$started_at)
feb2022$ended_at = as.character(feb2022$ended_at)
## test to see if it binds correctly. If it does, we remove it afterwards.
test1 = bind_rows(feb2022, jan2023)
rm(test1)
```

Afterwards, binded the data together and then remove the excess old data.

```r
# Binding Process ---------------------------------------------------------
annual_2022 = bind_rows(feb2022, mar2022, apr2022, may2022, jun2022,
jul2022, aug2022, sep2022, oct2022, nov2022, dec2022, jan2023)
# Remove Excess Data for the sake of storing the data
rm(feb2022,mar2022,apr2022,may2022,jun2022,jul2022,aug2022,sep2022,oct2022,
nov2022,dec2022,jan2023)
```

### Cleaning

Now that I have binded the data into one dataset, I wanted to filter out and simplify values. For example, I wanted to remove the NA and blank values, simplifying the value names, and adding some new columns, such as time. 

I limited the max time to 180 minutes given that the full-pass is limited to 3 hours before charging extra fees.

```r
# Cleaning out bad values -------------------------------------------------
## Removing any values that are the following. NA/NULL or blank, The end station being the repair shop
## Any value that is negative, or exceeds 3 hours.
annual_2022_v2 = annual_2022 %>%
filter(end_station_name != "Base - 2132 W Hubbard Warehouse") %>%
filter(![is.na](http://is.na/)(start_station_name) | start_station_name != "") %>%
filter(![is.na](http://is.na/)(start_station_id) | start_station_id != "") %>%
filter(![is.na](http://is.na/)(end_station_name) | end_station_name != "") %>%
filter(![is.na](http://is.na/)(end_station_id) | end_station_id != "") %>%
filter(between(as.double(ride_length), 0, 180))
```

Afterwards I created a new column called start_hour which grabs the hour of the time; this will be used to define the time of day.

```r
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
```

Finally, I wanted to simplify the words for graphical purposes

```r

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
```

### Datasets

After cleaning, I started to make various datasets depending on certain usage.

I started with creating basic_data_2022 to give a summary of various attributes between Casual vs Members. I also decided it is better to remove docked bikes altogether as it skews the value of ride_length for casuals since members never rode a docked bike.

```r
## Summary data based on Casual vs Member, Rideable Types
## Returns # of users, mean, median, quartile 25 + 75, std, 95% Conf 
## In of ride_length
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
```

Here is the output

![Screen Shot 2023-03-10 at 12.07.46 PM.png](https://s3-us-west-2.amazonaws.com/secure.notion-static.com/ea116954-a186-4557-a8b9-d33d5cc5ad48/Screen_Shot_2023-03-10_at_12.07.46_PM.png)

I then created a regular dataset called data_2022 because it contains more columns such as month, day, and time

```r
## Data organizes based on month, day, time, rideable_type
## Returns a mean, median, and # of users.
data_2022 = annual_2022_v2 %>%
filter(rideable_type != "Dock") %>%
group_by(month_of_day, day_of_week, time_of_day, member_casual, rideable_type) %>%
summarise(mean_ride = round(mean(ride_length),1),
median_ride = median(ride_length),
users = as.numeric(n()))
```

Here is the sample output

![Screen Shot 2023-03-10 at 12.10.11 PM.png](https://s3-us-west-2.amazonaws.com/secure.notion-static.com/89a41182-6cf7-454c-b514-4c4a60a48020/Screen_Shot_2023-03-10_at_12.10.11_PM.png)

I then created the rest of the datasets that would be used for the graphs

**********Note:********** The reason why I created ride_over_2022 is because the time-limit members ride on a bike-ride is 45 minutes and I want to see how many users ride over that 45 minutes. Meanwhile I also wanted to measure below 31 minutes because the one-ride pass gives 30 minutes for free for casual riders.

```r
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
```

# Summary and Recommendations

## Summary with Data

1. From February 2022-January 2023, I have observed a total of **********************4,425,213 rides**********************
    1. Where there are **1,594,234 casual riders or 36% of observed users.**
        1. Out of the casual riders, **55.9%** rode classic and ******44.1%****** rode electric
2. Casual Riders typically ride during the Weekends, Afternoon-Evening, and Summer Months
    1. Afternoon and Evening makes up **********78.6%********** of total rides
    2. The Most Popular Months are June, July August, September
        1. They make up **61.8%** of total rides 
    3. Least Popular Months are January, February, March, December
        1. They make up ********8.1%******** of total rides
3. Casual Riders, on average, rides longer than members 
    1. They typically ride on average **1.5 - 2x** more than members
    2. Especially high on certain months and day of week
        1. ie: **29.4 mins** during the afternoon of a Saturday in May 
    3. A good portion of casual riders ride above 45 minutes and under 31 minutes
        1. ******8.9%****** of casual riders ride above 45 minutes.
        2. **************83.6%************** of casual riders ride 30 or below minutes.
4. Location-wise, most of the casual riders start from the North-East side of Chicago
    1. There are 3 locations that have been used throughout the year
        1. Millenium Park, Streeter Dr & Grand Avenue, and Shedd Aquarium
    2. The map containing summer months indicate that most of the traffic for casual riders are concentrated in the top-right part of Chicago
    3. The map with the winter months indicate that traffic for casual riders are more spreadout across Chicago.

## Recommendations

1. Provide various packages and special offers within the membership plan.
    1. **Incremental Payments**
    Instead of charging $120 dollars up front annually, allow options to where a rider can pay $40 dollars up front every 4 months, that way when they purchase the annual plan, they will be paying only $40 upfront and pay $10 every month between months such as June - September which makes up **61.8%** of casual rides . That way if the user decides to pay for the rest of the year, they would eventually pay the $120 annual fee, but at least it would be less intimidating to start as it is a **66%** decrease of an upfront fee.
    2. **Location-Based Deals**
    You can do this by location-as well. Offer different deals or rewards for people who sign up as members at certain locations. Given that **Millenium Park, Streeter Dr & Grand Avenue, and Shedd Aquarium** stations are used throughout the year, it would make sense to prioritize those locations. That being said. Given that  **61.8%** of casual rides occur during the summer, it would make sense to put more emphasis of providing promotions in stations within the concentrated North-Eastern side of Chicago.
    3. **Month-Based Deals
    Only 8.1% of the casual rides occur during Winter,** thus could slightly increase the number of members if discounts were offered during that time period. Such as paying $10 with only a upfront cost of $20. While it is not ideal in terms of generating maximum profit, it will help create more potential customers as people tend to forget about what they are paying every month and soon the upfront cost might reset back to the original price after a given time.
2. Adjusting the Amount of both the Single Ride and Annual Membership
    1. **Increase Ride Limit of Annual Membership**
    Perhaps one of the biggest reasons to not go for an annual pass is that it is limited to **45 minutes** before the service starts charging you. That may not seem like a big deal, however at least ******8.9%****** of casual riders ride **above 45 minutes**. Being able to increase the limit to one hour or an hour and 15 minutes could bring those specific casual riders to become a member now that they have a reason to ride annually.
    2. **********************************************************Limit the Ride Limit or Increase cost of Single Ride**********************************************************
    While this is a risky recommendation, we do see that **************83.6%************** of casual riders ride **30 or below minutes**. This is a huge portion of the group as they take advantage of paying 1 **dollar for a 30 minute ride**, as advertised. Either Limit that ride to 20 minutes or increase the initial cost by a couple of dollars. Either way, they are still saving money if they are only going one way vs buying a day pass for $16.50

# Appendix: Charts and Results

## Number of Users

From February 2022-January 2023, I have observed a total of **********************4,425,213 rides**********************

I used the following to find how many members vs casuals on classic vs electric

```r
## Basic Data. Shows number of users given bike type
users_total = basic_data_2022 %>%
ggplot(aes(rideable_type, users, fill=member_casual)) +
scale_y_continuous(labels = scales::comma) +
labs(y= "Users", x = "Bike Type") + ggtitle("# of Users") +
stat_summary(geom = "bar", fun = "sum", position = "dodge")
```

![Screen Shot 2023-03-10 at 12.26.22 PM.png](https://s3-us-west-2.amazonaws.com/secure.notion-static.com/26ae9708-f4d3-482b-8421-1aafc1536876/Screen_Shot_2023-03-10_at_12.26.22_PM.png)

Where there are **1,594,234 casual riders or 36% of observed users.**

Out of the casual riders, **55.9%** rode classic and ******44.1%****** rode electric, indicating that electric bikes are indeed a popular form of transporation despite the extra cost.

Next I observed how many users given a particular time, day, and month

```r
## Number of Users during the time, day, and month -------------------------
###Shows how many members vs casuals on a given day during a certain time
users_day_2022 = data_2022 %>%
ggplot(aes(time_of_day, users, fill=member_casual)) +
scale_y_continuous(labels = scales::comma) +
labs(y= "Users", x = "Day") + facet_wrap(~day_of_week) +
ggtitle("# of Users per day") +
stat_summary(geom = "bar", fun = "sum", position = "dodge")

###Shows how many members vs casuals on a given day per month
users_month_2022 = data_2022 %>%
ggplot(aes(day_of_week, users, fill=member_casual)) +
scale_y_continuous(labels = scales::comma) +
labs(y= "Users", x = "Month") + facet_wrap(~month_of_day) +
ggtitle("# of Users per month") +
stat_summary(geom = "bar", fun = "sum", position = "dodge")
```

### **During the Week**

![Screen Shot 2023-03-10 at 12.32.26 PM.png](https://s3-us-west-2.amazonaws.com/secure.notion-static.com/44d91bc5-a3fc-4b6b-99e5-13d5873a1bca/Screen_Shot_2023-03-10_at_12.32.26_PM.png)

From here most casual riders ride during the afternoon and evening, especially on the weekends. 

### During the Month

![Screen Shot 2023-03-10 at 12.34.13 PM.png](https://s3-us-west-2.amazonaws.com/secure.notion-static.com/17d5b957-41fd-4dca-be16-fde5ee9be9be/Screen_Shot_2023-03-10_at_12.34.13_PM.png)

Most casual riders ride between **May and September,** but dwindles from **October to April**.

This is could be due to seasonal changes and vacation opportunities as people are less likely to bike for leisure during low temperatures or are simply not in Chicago for fun.

## Total Ride and Average Length

It is important to observe to see the total ride length as well as the average because maybe the limitations could be with the amount of ride time you can have on your membership pass, which is 45 minutes.

### During the Week

```r
### Shows the total ride time on a given day
total_day_2022 = ride_time_2022 %>%
ggplot(aes(factor(time_of_day), total, fill=member_casual)) +
scale_y_continuous(labels = scales::comma) + facet_wrap(~day_of_week) +
labs(y= "Time (in mins)", x = "Day") +
stat_summary(geom = "bar", fun = "sum", position = "dodge")
```

![Screen Shot 2023-03-10 at 12.51.49 PM.png](https://s3-us-west-2.amazonaws.com/secure.notion-static.com/03086386-c0d0-42e1-8c11-7404f73eff56/Screen_Shot_2023-03-10_at_12.51.49_PM.png)

It is seen that during the weekdays, Member typically have more total riding time vs casuals meanwhile Casual riders ride a lot more than members during the weekends.

```r
### Shows average time riding during on a given day
average_day_2022 = data_2022 %>%
ggplot(aes(factor(time_of_day), mean_ride, fill=member_casual)) +
geom_bar(stat = "identity", position="dodge") +
labs(y= "Time (in mins)", x = "Day") + facet_wrap(~day_of_week) +
ggtitle("Average Ride Length Per Day")
```

![Screen Shot 2023-03-10 at 12.54.38 PM.png](https://s3-us-west-2.amazonaws.com/secure.notion-static.com/75c90827-b58e-4b73-9719-f43d1aef3c01/Screen_Shot_2023-03-10_at_12.54.38_PM.png)

Looking at the average, however, we do see that overall, casual riders ride alot longer than members and that could be due to the usage of the bike. Many members could be using the bikes to go to work, thus has a much shorter average time vs casual riders.

### During the Month

```r
### Shows the total ride time on a given month
total_month_2022 = ride_time_2022 %>%
ggplot(aes(factor(day_of_week), total, fill=member_casual)) +
scale_y_continuous(labels = scales::comma) + facet_wrap(~month_of_day) +
labs(y= "Time (in mins)", x = "Day") +
ggtitle("Total Ride Length Per Month") +
stat_summary(geom = "bar", fun = "sum", position = "dodge")
```

![Screen Shot 2023-03-10 at 12.57.06 PM.png](https://s3-us-west-2.amazonaws.com/secure.notion-static.com/35dbbba5-98e2-415d-bff5-79c443cdcb28/Screen_Shot_2023-03-10_at_12.57.06_PM.png)

While on an everyday basis, casual riders have less total riding time, they dominate it across months as they easily surpass members in the popular months (May, June, July, and August)

```r
### Shows average time riding during on a given month
average_month_2022 = data_2022 %>%
ggplot(aes(day_of_week, mean_ride, fill=member_casual)) +
geom_bar(stat = "identity", position="dodge") +
labs(y= "Time (in mins)", x = "Day") + facet_wrap(~month_of_day) +
ggtitle("Average Ride Length Per Month")
```

![Screen Shot 2023-03-10 at 12.59.33 PM.png](https://s3-us-west-2.amazonaws.com/secure.notion-static.com/f9c0dba9-ef5a-4d24-ba5f-e55853fda582/Screen_Shot_2023-03-10_at_12.59.33_PM.png)

Similar to its day counterpart, the Average Ride Length for Casual Riders is much higher compared to members. Almost twice as much every month.

### Riding Over 45 Mins and 30 minutes and under

Which made me curious to check one thing. How many casual riders ride over 45 minutes vs members?

```r
### Looks at how many users ride their bikes over 45 minutes
ride_over_stat_2022 = ride_over_2022 %>%
ggplot(aes(factor(rideable_type), over, fill=member_casual)) +
scale_y_continuous(labels = scales::comma) +
labs(y= "Users", x = "Ride Type") +
ggtitle("# of Users over 45 mins of Ride Type") +
stat_summary(geom = "bar", fun = "sum", position = "dodge")
```

![Screen Shot 2023-03-10 at 1.55.26 PM.png](https://s3-us-west-2.amazonaws.com/secure.notion-static.com/54d58aba-8d72-4934-bac4-649218db01c7/Screen_Shot_2023-03-10_at_1.55.26_PM.png)

As observed, there are much more casual riders riding over 45 minutes compared to members.

```r
### Looks at how many users ride their bikes under 31 minutes.
ride_under_stat_2022 = ride_over_2022 %>%
ggplot(aes(factor(rideable_type), under, fill=member_casual)) +
scale_y_continuous(labels = scales::comma) +
labs(y= "Users", x = "Ride Type") +
ggtitle("# of Users 30 mins and under of Ride Type") +
stat_summary(geom = "bar", fun = "sum", position = "dodge")
```

However, when I checked the 30 minutes and under. The amount of casual users is much greater as it could be due to the advantage of getting the first 30 minutes for free.

![Screen Shot 2023-03-10 at 1.56.37 PM.png](https://s3-us-west-2.amazonaws.com/secure.notion-static.com/3f5b4a73-1f5d-4d66-add7-cb0fc3b79d84/Screen_Shot_2023-03-10_at_1.56.37_PM.png)

### During the Month

One of the last things I wanted to check if there is an indication that casual riders who ride over 45 minutes ride mostly during the summer

```r
ride_over_month_2022 = ride_over_2022 %>%
ggplot(aes(factor(rideable_type), over, fill=member_casual)) +
geom_bar(stat = "identity", position = "dodge") +
scale_y_continuous(labels = scales::comma) + facet_wrap(~month_of_day) +
labs(y= "Users", x = "Ride Type") +
ggtitle("# of Users over 45 mins of Ride Type Per Month")
```

![Screen Shot 2023-03-10 at 1.15.24 PM.png](https://s3-us-west-2.amazonaws.com/secure.notion-static.com/8fee114c-e575-4f33-a2b7-aa358e0ffde1/Screen_Shot_2023-03-10_at_1.15.24_PM.png)

This turned out to be correct as it occurs similar to a bell curve, placing heavy influence onto the months occurring during the summer.

## Location

The last thing I want to test is to see where do casual riders frequent when riding their bikes.

Thus I went over to excel to find out which Location is used throughout the year and where it has the most users.

![Screen Shot 2023-03-10 at 1.18.28 PM.png](https://s3-us-west-2.amazonaws.com/secure.notion-static.com/16a31156-de19-47ed-8f62-2db5882ebf05/Screen_Shot_2023-03-10_at_1.18.28_PM.png)

So far there are 4 stations that are fairly consistent in having people ride from that location, where Streeter Drive & Grand Avenue has an exceptionally high amount of users. 

******************************************************************************************************************************************Note: I only grabbed the location from a single set of Lat, and Long,****************************************************************************************************************************************** **************************thus the total number is much smaller than expected.************************** 

I then went over to Google Maps to pinpoint where these locations are 

![All Casual.png](https://s3-us-west-2.amazonaws.com/secure.notion-static.com/b189ca32-882e-4cc3-9624-63f9946a6c7c/All_Casual.png)

************************************************************************************************************************************Locations in Black are intersected and other colors represent singular locations from certain months.************************************************************************************************************************************

Most of the stations casual riders frequent is among the North East Side of Chicago with a few outliers in the South Side.

So when we look into the most popular months vs least popular months, it is consistent with this concentration

### Popular Months

This includes June, July August, September

![Screen Shot 2023-03-10 at 12.11.24 AM.png](https://s3-us-west-2.amazonaws.com/secure.notion-static.com/71e22f91-cb0f-4260-b179-593c6f8d6e22/Screen_Shot_2023-03-10_at_12.11.24_AM.png)

These focus more on the concentrated top-right area.

### Least Popular Months

This includes January 2023, February, March, December

![Screen Shot 2023-03-10 at 12.08.12 AM.png](https://s3-us-west-2.amazonaws.com/secure.notion-static.com/7a3758ab-261c-41b1-a849-297c0f99e219/Screen_Shot_2023-03-10_at_12.08.12_AM.png)

Less concentrated and focused more on the outliers in the South

Fin.
