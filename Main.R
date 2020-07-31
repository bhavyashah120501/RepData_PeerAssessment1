
library(lubridate)
library(ggplot2)
if(!file.exists('data.zip')){
  download.file('https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip' , 'data.zip')
  unzip('data.zip')
}

activity <- read.csv('activity.csv')
stepsperday <-  with(activity , aggregate(steps ~ date , FUN = sum ))
g <- ggplot(stepsperday ,aes(steps)) + geom_histogram(binwidth = 2500 ,fill='lightblue' , col = 'darkblue') + labs(title='histogram of total number of  steps per day')
print(g)
dev.copy(png , 'plot1.png')
dev.off()
means <- mean(stepsperday$steps)
medians <- median(stepsperday$steps)


meanstepsperinterval <- with(activity , aggregate(steps ~ interval , FUN = mean , na.rm=TRUE))
g <- g <- ggplot(meanstepsperinterval , aes(interval , steps)) + geom_line() + labs(x = 'Date' , y = 'Average Steps' , title ='Average steps per interval')
sum(is.na(activity$steps))
print(g)
dev.copy(png , 'plot2.png')
dev.off()

maxAverageSteps <- meanstepsperinterval$steps[which.max(meanstepsperinterval$steps)]
maxAverageStepsInterval <- meanstepsperinterval$interval[which.max(meanstepsperinterval$steps)]

NArows <- with(activity,sum(is.na(steps)))

activity$steps <- ifelse(is.na(activity$steps) , meanstepsperinterval$steps[match(meanstepsperinterval$interval,activity$interval)] , activity$steps)

sum(is.na(activity$steps))
stepsperday2 <-  with(activity , aggregate(steps ~ date , FUN = sum ))
g <- ggplot(stepsperday2 ,aes(steps)) + geom_histogram(binwidth = 2500 ,fill='lightblue' , col = 'darkblue') + labs(title='histogram of total number of  steps per day')
print(g)
dev.copy(png , 'plot3.png')
dev.off()

y <- day(activity$date)
weekends <- y== 7 | y==1
weekday <- y!= 7 & y!=1
y[weekends] = 1
y[weekday] = 2
activity$classifiy <- y
z <- with(activity,aggregate(steps ~ interval + classifiy , FUN = mean))
z$classifiy <- factor(z$classifiy , labels = c('weekend' , 'weekday'))
g <- ggplot(z , aes(interval , steps)) + geom_line(aes(col = classifiy)) + labs(title='Average steps per interval')
print(g)
dev.copy(png , 'plot4.png')
dev.off()