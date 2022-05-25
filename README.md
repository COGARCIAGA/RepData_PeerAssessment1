# RepData_PeerAssessment1
Project 1 Reproducible Research
---
title: "Reproducible Research project 1"
author: "Gabriel Garcia"
date: "24/5/2022"
output: html_document
---
##Assignment Instructions

Introduction
It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

Assignment
This assignment will be described in multiple parts. You will need to write a report that answers the questions detailed below. Ultimately, you will need to complete the entire assignment in a single R markdown document that can be processed by knitr and be transformed into an HTML file.

Throughout your report make sure you always include the code that you used to generate the output you present. When writing code chunks in the R markdown document, always use echo = TRUE so that someone else will be able to read the code. This assignment will be evaluated via peer assessment so it is essential that your peer evaluators be able to review the code for your analysis.

For the plotting aspects of this assignment, feel free to use any plotting system in R (i.e., base, lattice, ggplot2)

Fork/clone the GitHub repository created for this assignment. You will submit this assignment by pushing your completed files into your forked repository on GitHub. The assignment submission will consist of the URL to your GitHub repository and the SHA-1 commit ID for your repository state.

1.Code for reading in the dataset and/or processing the data
2.Histogram of the total number of steps taken each day
3.Mean and median number of steps taken each day
4.Time series plot of the average number of steps taken
5.The 5-minute interval that, on average, contains the maximum number of steps
6.Code to describe and show a strategy for imputing missing data
7.Histogram of the total number of steps taken each day after missing values are imputed
8.Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
9.All of the R code needed to reproduce the results (numbers, plots, etc.) in the report

Questions to be answered:

What is mean total number of steps taken per day?
What is the average daily activity pattern?
Imputing missing values
Are there differences in activity patterns between weekdays and weekends?

## Step 1
## Code for reading in the dataset and/or processing the data
```{r, echo = TRUE}
setwd("C:/Users/Gabriel.Garcia/OneDrive/Data science Course/Reproducible Research/Course project 1")
Activity_URL <- "C:/Users/Gabriel.Garcia/OneDrive/Data science Course/Reproducible Research/Course project 1/repdata_data_activity.zip"
unzip("repdata_data_activity.zip")
activity <- read.csv("activity.csv")
```

## understading Activity file
```{r}
### this is the understanding of activity file and its figures & characteristics
dim(activity)
head(activity)
summary(activity)
names(activity)
str(activity)
#total number of missing data
sum(is.na(activity$steps))/dim(activity)[[1]]
#transforming the date column into date format using lubridate
library(lubridate)
activity$date<-ymd(activity$date)
length(unique(activity$date))
activity$date <- as.POSIXct(activity$date, "%Y-%m-%d")
weekday <- weekdays(activity$date)
activity <- cbind(activity,weekday)
```

## Step 2
## Histogram of the total number of steps taken each day
```{r, echo = TRUE}
### What is mean total number of steps taken per day?
###Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
library(ggplot2)
Factor_Date <- as.factor(activity$date)
activity_total_steps <- with(activity,
                             aggregate(steps,
                                       by = list(date), FUN = sum, na.rm = TRUE))
names(activity_total_steps) <- c("moment", "action")
### Histogram of the total number of steps taken each day
png("steps_total_steps_plot1.png")
ggplot(activity_total_steps,aes(y = action , x = moment), col= "red")+
        geom_bar(stat="identity")+
        theme_bw() + guides(fill="none")+
        ylab("Total Steps")+
        xlab("Date")+
        ggtitle("Total number of steps taken per day")
dev.off()
```

## Step 3
## Mean and median number of steps taken each day
```{r, echo = TRUE}
library(dplyr)
### Mean and median number of steps taken each day
### 
mean(activity$steps,na.rm = TRUE)
activity_means_steps_per_date <- with(activity,
                             aggregate(steps,
                                       by = list(date), FUN = mean, na.rm = TRUE))
names(activity_means_steps_per_date) <- c("moment", "action")
median(activity$steps, na.rm = TRUE)
activity_media_steps_per_date<- with(activity,
                             aggregate(steps,
                                       by = list(date), FUN = median, na.rm = TRUE))
names(activity_media_steps_per_date) <- c("moment", "action")
```

## Step 4
## Time series plot of the average number of steps taken
```{r, echo = TRUE}
activity_means_steps_per_day <- with(activity,
                                     aggregate(steps,
                                               by = list(weekday), FUN = mean, na.rm = TRUE))
names(activity_means_steps_per_day) <- c("moment", "action")

### The 5-minute interval that, on average, contains the maximum number of steps
activity_means_steps_per_interval <- with(activity,
                                      aggregate(interval,
                                                by = list(steps), FUN = mean, na.rm = TRUE))
names(activity_means_steps_per_interval) <- c("interval", "steps")
activity_means_steps_per_interval <- activity_means_steps_per_interval[with(activity_means_steps_per_interval, order(-activity_means_steps_per_interval$steps)),]
```

## Step 5
## The 5-minute interval that, on average, contains the maximum number of steps
```{r, echo = TRUE}
activity_means_steps_per_interval <- with(activity,
                                      aggregate(interval,
                                                by = list(steps), FUN = mean, na.rm = TRUE))
names(activity_means_steps_per_interval) <- c("interval", "steps")
activity_means_steps_per_interval <- activity_means_steps_per_interval[with(activity_means_steps_per_interval, order(-activity_means_steps_per_interval$steps)),]
```

## Step 6
## Code to describe and show a strategy for imputing missing data
##clean activity file without NA
```{r, echo = TRUE}
#total number of missing data
sum(is.na(activity$steps))/dim(activity)[[1]]
activity_clean <- na.exclude(activity)
##clean activity file without NA
activity_clean <- na.exclude(activity)
```

## Step 7
## Histogram of the total number of steps taken each day after missing values are imputed
```{r, echo = TRUE}
Factor_Date <- as.factor(activity_clean$date)
activity_total_steps <- with(activity_clean,
                             aggregate(steps,
                                       by = list(date), FUN = sum, na.rm = TRUE))
names(activity_total_steps) <- c("moment", "action")
## Histogram of the total number of steps taken each day without NA
png("steps_total_steps_plot1.png")
ggplot(activity_total_steps,aes(y = action , x = moment), col= "red")+
        geom_bar(stat="identity")+
        theme_bw() + guides(fill="none")+
        ylab("Total Steps")+
        xlab("Date")+
        ggtitle("Total number of steps taken per day")
dev.off()
```

## Step 8
## Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
## Are there differences in activity patterns between weekdays and weekends?
```{r, echo = TRUE}
Factor_Date <- as.factor(activity_clean$date)
activity_total_steps_weekday <- with(activity_clean,
                             aggregate(steps,
                                       by = list(weekday), FUN = sum, na.rm = TRUE))
names(activity_total_steps_weekday) <- c("weekday", "action")
activity_total_steps_weekday <- activity_total_steps_weekday[with(activity_total_steps_weekday, order(activity_total_steps_weekday$weekday)),]

### separate weekday - weekend
activity_clean$datetype <- sapply(activity_clean$date, function(x) {
        if (weekdays(x) == "Sábado" | weekdays(x) =="domingo") 
        {y <- "Weekend"} else 
        {y <- "Weekday"}
        y
})

### Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
activity_different_steps_type_day <- with(activity_clean,
                                     aggregate(steps,
                                               by = list(datetype, interval), FUN = sum, na.rm = TRUE))
names(activity_different_steps_type_day) <- c("typeofday", "interval","action")
png("activity_different_steps_type_day.png")
ggplot(activity_different_steps_type_day,aes(y = action, x = interval, fill = typeofday))+
        geom_bar(stat="identity")+
        theme_bw() + guides(fill="none")+
        facet_grid(.~typeofday,scales ="free",space="free") +
        ylab("Total Steps")+
        xlab("Type of Date")+
        ggtitle("Total number of steps taken per Type of Date")
dev.off()
