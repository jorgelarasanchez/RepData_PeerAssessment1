---
title: "Reproducible Research: Peer Assessment 1"
author: "JLara"
date: "15 de abril de 2017"
keep_md: true
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Introduction

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site:

Dataset: Activity monitoring data [52K]
The variables included in this dataset are:

steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
date: The date on which the measurement was taken in YYYY-MM-DD format
interval: Identifier for the 5-minute interval in which measurement was taken
The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

# Loading and preprocessing the data

* Load the data

```{r Load the data}
fileURL <- 'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip'
dataFileZip <- "./repdata_Fdata_Factivity.zip"

# Download the file
if (file.exists(dataFileZip) == FALSE) {
  download.file(fileURL,destfile=dataFileZip ,method="curl")
  # Unzip the file
  unzip(zipfile=dataFileZip ,exdir=".")
}

activity <- read.csv("activity.csv")
```

* Process/transform the data (if necessary) into a format suitable for your analysis

```{r summary}
summary(activity)
```

```{r str}
str(activity)
```

We convert the "date" column of activity to the appropriate format

```{r date}
activity$date <- as.Date(activity$date, "%Y-%m-%d")
```
We look for in date the day of the week for later question

```{r weekend}
Sys.setlocale(category = "LC_ALL", locale = "english")
activity <- data.frame(date=activity$date, 
                           day=tolower(weekdays(activity$date)), 
                           steps=activity$steps, 
                           interval=activity$interval)

activity <- cbind(activity, 
                      weekday=ifelse(activity$day == "saturday" | 
                                     activity$day == "sunday", "weekend", 
                                     "weekday"))

activity <- data.frame(date=activity$date, 
                       day=activity$day, 
                       weekday=activity$weekday, 
                       interval=activity$interval,
                       steps=activity$steps)
head(activity)
```


## What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.

1.Calculate the total number of steps taken per day

```{r total number of steps}
steps_dates <- tapply(activity$steps, activity$date, sum,na.rm=TRUE)
```

2.Make a histogram of the total number of steps taken each day

```{r histogram}
hist(steps_dates, xlab = 'Total steps per day', main = ' Total number of steps taken each day', col = 'red', breaks=10)
```

3.Calculate and report the mean and median of the total number of steps taken per day  

```{r mean}
mean(steps_dates)
```

```{r median}
median(steps_dates)
```



## What is the average daily activity pattern?

1.Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```{r seres plot}
steps_interval <- tapply(activity$steps, activity$interval, mean,na.rm=TRUE)

plot(steps_interval ~ unique(activity$interval), type="l", xlab = "5-min interval",ylab="averaged across all days",main="average daily activity pattern")
```

2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r maximum number of steps}
steps_interval[which.max(steps_interval)]
```


## Imputing missing values

1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```{r missing values}
sum(is.na(activity$steps))
```

2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

* The NA values are replaced by the mean of 5 minutes interval

```{r strategy}
Sys.setlocale(category = "LC_ALL", locale = "english")
new_activity <- activity

for (i in 1:nrow(activity)){
  if(is.na(activity$steps[i])){
    new_activity$steps[i]<- steps_interval[[as.character(activity[i, "interval"])]]
  }
}
```

3.Create a new dataset that is equal to the original dataset but with the missing data filled in.

```{r new dataset}
head(new_activity) 
```

4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```{r total number of steps2}
steps_dates_new <- tapply(new_activity$steps, new_activity$date, sum,na.rm=TRUE)
```
```{r histogram2}
hist(steps_dates_new, xlab = 'Total steps per day', main = ' Total number of steps taken each day', col = 'red', breaks=10)
```

mean with NA estimated 

```{r mean2}
mean(steps_dates_new)
```

median with NA estimated 

```{r median2}
median(steps_dates_new)
```

## Are there differences in activity patterns between weekdays and weekends?

1.Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```{r new factor}
head(new_activity)
```

2.Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```{r panel plot}
library(lattice)
Sys.setlocale(category = "LC_ALL", locale = "english")
avg_data <-  aggregate(new_activity$steps, 
                       by=list(new_activity$weekday, 
                               new_activity$day, new_activity$interval), mean)

names(avg_data) <- c("day", "weekday", "interval", "mean")  

xyplot(mean ~ interval | day, avg_data, 
       type="l", 
       lwd=1, 
       xlab="5-minute interval", 
       ylab="Number of steps",
       layout=c(1,2))
```
