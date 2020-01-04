library(ggplot2)
library(ggthemes)
library(lubridate)
library(dplyr)
library(tidyr)
library(DT)
library(scales)

setwd("C:/Users/DELL/Desktop/Uber-dataset/Uber rides analysis")
getwd()

#creating vector of colors
colors = c("#CC1011", "#665555", "#05a399", "#cfcaca", "#f5e840", "#0683c9", "#e075b0")

#reading data

data_april = read.csv('uber-raw-data-apr14.csv')
data_may = read.csv('uber-raw-data-may14.csv')
data_june = read.csv('uber-raw-data-jun14.csv')
data_july = read.csv('uber-raw-data-jul14.csv')
data_august = read.csv('uber-raw-data-aug14.csv')
data_september = read.csv('uber-raw-data-sep14.csv')

dataset = rbind(data_april, data_may, data_june, data_july, data_august, data_september)
dataset$Date.Time <- as.POSIXct(dataset$Date.Time, format = "%m/%d/%Y %H:%M:%S")
dataset$Time <- format(as.POSIXct(dataset$Date.Time, format = "%m/%d/%Y %H:%M:%S"), format="%H:%M:%S")
dataset$Date.Time <- ymd_hms(dataset$Date.Time)

dataset$day <- factor(day(dataset$Date.Time))
dataset$month <- factor(month(dataset$Date.Time, label = TRUE))
dataset$year <- factor(year(dataset$Date.Time))
dataset$dayofweek <- factor(wday(dataset$Date.Time, label = TRUE))
dataset$hour <- factor(hour(hms(dataset$Time)))
dataset$minute <- factor(minute(hms(dataset$Time)))
dataset$second <- factor(second(hms(dataset$Time)))

summary(dataset)

#plotting the trips

hour_data <- dataset %>% group_by(hour) %>% dplyr::summarise(Total = n())
datatable(hour_data)
ggplot(hour_data, aes(hour, Total)) + geom_bar(stat = "identity", fill = "yellow", color = "black") + ggtitle("Trips every hour") + theme(legend.position = "none") + scale_y_continuous(labels = comma)

month_hour <- dataset %>% group_by(month, hour) %>% dplyr::summarise(Total = n())
ggplot(month_hour, aes(hour, Total, fill = month)) + geom_bar(stat = "identity") + ggtitle("Trips by hour and month") + scale_y_continuous(labels = comma)
  
day_group <- dataset %>% group_by(day) %>% dplyr::summarise(Total = n())
datatable(day_group)
ggplot(day_group, aes(day, Total)) + geom_bar(stat = "identity", fill = "steelblue") + ggtitle("Trips every day") + theme(legend.position = "none") + scale_y_continuous(labels = comma)

day_month_group <- dataset %>% group_by(month, day) %>% dplyr::summarise(Total = n())
ggplot(day_month_group, aes(day, Total, fill = month)) + geom_bar(stat = "identity") + ggtitle("Trips by day and month") + scale_y_continuous(labels = comma) + scale_fill_manual(values = colors)

month_group <- dataset %>% group_by(month) %>% dplyr::summarise(Total = n())
datatable(month_group)
ggplot(month_group, aes(month, Total, fill = month)) + geom_bar(stat = "identity") + ggtitle("Trips by month") + theme(legend.position = "none") + scale_y_continuous(labels = comma) + scale_fill_manual(value = colors)

month_weekday <- dataset %>% group_by(month, dayofweek) %>% dplyr::summarise(Total = n())
ggplot(month_weekday, aes(month, Total, fill = dayofweek)) + geom_bar( stat = "identity", position = "dodge") + ggtitle("Trips by Day and Month") + scale_y_continuous(labels = comma) + scale_fill_manual(values = colors)

#number of trips by Base
ggplot(dataset, aes(Base)) + geom_bar(fill = "darkred") + scale_y_continuous(labels = comma) + ggtitle("Trips by base")

ggplot(dataset, aes(Base, fill = month)) + geom_bar(position = "dodge") + scale_y_continuous(labels = comma) + ggtitle("Trips by Bases and month") + scale_fill_manual(values = colors)

ggplot(dataset, aes(Base, fill = dayofweek)) + geom_bar(position = "dodge") + scale_y_continuous(labels = comma) + ggtitle("Trips by Bases and dayofweek") + scale_fill_manual(values = colors)

#Heatmap visualization of day, hour and month
day_and_hour <- dataset %>% group_by(day, hour) %>% dplyr::summarise(Total = n())
datatable(day_and_hour)
ggplot(day_and_hour, aes(day, hour, fill = Total)) + geom_tile(color = "white") + ggtitle("Heat Map by Hour and Day")
ggplot(day_month_group, aes(day, month, fill = Total)) + geom_tile(color = "white") + ggtitle("Heat Map by Month and Day")
ggplot(month_weekday, aes(dayofweek, month, fill = Total)) + geom_tile(color = "white") + ggtitle("Heat Map by Month and Day Of Week")

month_base <- dataset %>% group_by(Base, month) %>% dplyr::summarise(Total=n())
dayofweek_base <- dataset %>% group_by(Base, dayofweek) %>% dplyr::summarise(Total=n())

ggplot(month_base, aes(Base, month, fill = Total)) + geom_tile(color = "white") + ggtitle("Heat Map by Month and Base")
ggplot(dayofweek_base, aes(Base, dayofweek, fill = Total)) + geom_tile(color = "white") + ggtitle("Heat Map by Day of week and Base")

#Creating visualization map of rides in New York
min_lat <- 40.5774
max_lat <- 40.9176
min_long <- -74.15
max_long <- -73.7004

ggplot(dataset, aes(x=Lon, y=Lat)) + geom_point(size=1, color="blue") + scale_x_continuous(limits = c(min_long, max_long)) + scale_y_continuous(limits = c(min_lat, max_lat)) + theme_map() + ggtitle("NYC MAP BASED ON UBER RIDES DURING 2014 (APR-SEP)")
ggplot(dataset, aes(x=Lon, y=Lat, color = Base)) + geom_point(size=1) + scale_x_continuous(limits = c(min_long, max_long)) + scale_y_continuous(limits = c(min_lat, max_lat)) + theme_map() + ggtitle("NYC MAP BASED ON UBER RIDES DURING 2014 (APR-SEP) by BASE")

