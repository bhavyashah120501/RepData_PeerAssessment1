---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
library(ggplot2)
if(!file.exists("data.zip")){
  download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", "data.zip")
  unzip("data.zip")
}

activity <- read.csv("activity.csv")
```


## What is mean total number of steps taken per day?

```r
stepsperday <-  with(activity , aggregate(steps ~ date , FUN = sum ))
means <- mean(stepsperday$steps)
medians <- median(stepsperday$steps)
```


## What is the average daily activity pattern?

Average Daily Patterns

```r
library(ggplot2)
g <- ggplot(stepsperday ,aes(steps)) + geom_histogram(binwidth = 2500 ,fill="lightblue" , col = "darkblue") + labs(title="histogram of total number of  steps per day")
png("plot1.png")
```


Average Pattern  per interval

```r
library(ggplot2)
meanstepsperinterval <- with(activity , aggregate(steps ~ interval , FUN = mean , na.rm=TRUE))
g <- g <- ggplot(meanstepsperinterval , aes(interval , steps)) + geom_line() + labs(x = "Date" , y = "Average Steps" , title ="Average steps per interval")
maxAverageSteps <- meanstepsperinterval$steps[which.max(meanstepsperinterval$steps)]
maxAverageStepsInterval<-meanstepsperinterval$interval[which.max(meanstepsperinterval$steps)]
png("plot2.png")
```



## Imputing missing values

```r
NArows <- with(activity,sum(is.na(steps)))

activity$steps <- ifelse(is.na(activity$steps) , meanstepsperinterval$steps[match(meanstepsperinterval$interval,activity$interval)] , activity$steps)
```

Change in mean Steps per day

```r
library(ggplot2)
stepsperday2 <-  with(activity , aggregate(steps ~ date , FUN = sum ))
g <- ggplot(stepsperday2 ,aes(steps)) + geom_histogram(binwidth = 2500 ,fill="lightblue" , col = "darkblue") + labs(title="histogram of total number of  steps per day")
png("plot3.png")
```

## Are there differences in activity patterns between weekdays and weekends?

```r
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following objects are masked from 'package:base':
## 
##     date, intersect, setdiff, union
```

```r
library(ggplot2)
y <- day(activity$date)
weekends <- y== 7 | y==1
weekday <- y!= 7 & y!=1
y[weekends] = 1
y[weekday] = 2
activity$classifiy <- y
z <- with(activity,aggregate(steps ~ interval + classifiy , FUN = mean))
z$classifiy <- factor(z$classifiy , labels = c("weekend" , "weekday"))
g <- ggplot(z , aes(interval , steps)) + geom_line(aes(col = classifiy)) + labs(title="Average steps per interval")
png("plot4.png")
```
