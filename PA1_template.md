---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
Load libraries to be used in analysis

```r
library(dplyr)
library(ggplot2)
```


Read in the data and view a summary

```r
allData <- read.csv("activity.csv")
summary(allData)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

Group data by day

```r
dayData <- allData %>% group_by(date) %>% summarize(sumSteps = sum(steps))
```

Group data by 5-minute interval

```r
intervalData <- allData %>% group_by(interval) %>% 
    summarize(meanSteps = mean(steps, na.rm = TRUE))
```

## What is mean total number of steps taken per day?

Histogram for the total number of steps taken per day

```r
hist(dayData$sumSteps, breaks = 10, col = "blue", main = "Total Number of Steps 
     per Day", xlab = "Steps")
```

![](PA1_template_files/figure-html/totalSteps-1.png)<!-- -->

Calculate the mean and median total number of steps taken per day

```r
meanSteps <- mean(dayData$sumSteps, na.rm = TRUE)
medianSteps <- median(dayData$sumSteps, na.rm = TRUE)
```

Mean steps per day: 10,766.19  
Median steps per day: 10,765

## What is the average daily activity pattern?

Time series plot for the average number of steps taken per 5-minute interval

```r
ggplot(intervalData, aes(interval, meanSteps)) +
geom_line(size = 1, color = "blue") +
labs(title = "Average Daily Steps per 5-Minute Interval", y = "Avg. Steps", 
        x = "5-Minute Interval")
```

![](PA1_template_files/figure-html/meanStepsIntervalPlot-1.png)<!-- -->

Find the 5-minute interval with the highest average daily step value

```r
maxInterval <- intervalData$interval[which.max(intervalData$meanSteps)]
maxSteps <- max(intervalData$meanSteps)
```

Interval with max average steps: 835  

## Imputing missing values

Calculate the total number of missing rows in the dataset

```r
missingRows <- sum(is.na(allData$steps))
```

Missing rows: 2,304

Imputation function: replace NAs with the mean for that 5-minute interval

```r
impute <- function(x) {
    missing <- is.na(x$steps)
    x$steps[missing] <- intervalData$meanSteps
    return (x)
}
```

Create a dataset equal to the original but with missing values filled in

```r
filledData <- impute(allData)
```

Histogram for the total number of steps taken per day with imputed data

```r
filledDayData <- filledData %>% group_by(date) %>% summarize(sumSteps = sum(steps))

hist(filledDayData$sumSteps, breaks = 10, col = "blue", main = "Total Number of Steps 
     per Day - Filled Data", xlab = "Steps")
```

![](PA1_template_files/figure-html/totalFilledSteps-1.png)<!-- -->

Calculate the mean and median total number of steps taken per day with imputed data

```r
filledMeanSteps <- mean(filledDayData$sumSteps, na.rm = TRUE)
filledMedianSteps <- median(filledDayData$sumSteps, na.rm = TRUE)
```

Mean steps per day: 10,766.19  
Median steps per day: 10,766.19

After filling in the missing values with the average number of daily steps for 
that 5-minute interval, the mean steps per day is unchanged, and the median steps
per day is now equal to the mean. The eight days with filled data all fall into 
the largest bin in the histogram, as they all have 10,766 daily steps.

## Are there differences in activity patterns between weekdays and weekends?

Add variable indicating whether a given date is a weekday or weekend to dataset

```r
days <- weekdays(as.Date(filledData$date))
weekday <- c()
for (day in days) {
    if (day == "Saturday" | day == "Sunday") {weekday <- c(weekday, "weekend")}
    else {weekday <- c(weekday, "weekday")}
}

weekdayFilledData <- filledData %>% mutate(weekday = as.factor(weekday))
```


Time series plot for the average number of steps taken per 5-minute interval, 
broken down by weekday/weekend

```r
intervalFilledData <- weekdayFilledData %>% group_by(interval, weekday) %>% 
    summarize(meanSteps = mean(steps, na.rm = TRUE))

ggplot(intervalFilledData, aes(interval, meanSteps)) +
    geom_line(size = 1, color = "blue") +
    facet_grid(weekday~.) +
    labs(title = "Average Daily Steps per 5-Minute Interval - Weekdays and Weekends", 
        y = "Avg. Steps", x = "5-Minute Interval")
```

![](PA1_template_files/figure-html/panelPlot-1.png)<!-- -->

The plot above shows that activity is more consistent throughout the day on weekends,
while there is a significant spike in the morning on weekdays.
