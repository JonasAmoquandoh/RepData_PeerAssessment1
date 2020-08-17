---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
mydata <- read.csv("activity.csv")    <!-- Load data file -->

mydata$date <- as.Date(mydata$date)   <!-- process data to convert date column to date class -->


## What is mean total number of steps taken per day?
cleanData <- mydata[!is.na(mydata$steps), ]  <!-- make a new dataframe without NA values -->
groupedByDate <- ddply(cleanData, ~date, summarise, sum = sum(steps))  <!-- group values by date -->
hist(groupedByDate$sum, xlab = "Total Steps", main = "Histogram total steps per day", col = "darkmagenta")   <!-- histogram showing the spread of the distribution -->


The **mean** of the distribution is: 10766.19
and its **median** is: 10765



## What is the average daily activity pattern?
groupedByInterval <- ddply(cleanData, ~interval, summarise, mean = mean(steps))  <!-- groupvalues by interval -->
with(groupedByInterval, plot(interval, mean, type = "l", ylab = "Average number of steps", xlab = "Interval", main = "Average daily activity", col = "darkred"))  <!-- Time series showing average daily interval activity -->

maxVal <- max(groupedByInterval$mean)   <!-- Maximum value of the time series -->
print(maxVal)


The maximum number of steps (on average across all the days) is maxVal: 206.1698
and it is contained in interval maxInterval: 835


## Imputing missing values
sum(is.na(mydata$steps))  <!-- calculate sum of missing values -->


### Since there are missing values in the Dataset, they need to be filled and I do that by substituting the mean
newdata <- mydata   <!-- create new dataset -->
missingsteps <- is.na(newdata$steps)  <!-- locate tha NAs -->


<!-- convert interval(s) to factor(s) -->
newdata$interval <- factor(newdata$interval)

groupedByInterval$interval <- factor(groupedByInterval$interval)

newdata[missingsteps, "steps"] <- groupedByInterval[newdata[missingsteps, "interval"], "mean"]            <!-- fill any missing step with mean -->



### Reconstruct the histogram and compare your results
groupedByDate2 <- ddply(newdata, ~date, summarise, sum = sum(steps))

hist(groupedByDate2$sum, xlab = "Total Steps", main = "Histogram total steps per day", col = "darkgreen")

mean(groupedByDate2$sum)

median(groupedByDate2$sum)

We notice that mean stays the same and median increases by one
However as we see from the histogram the impact is very 'light'



## Are there differences in activity patterns between weekdays and weekends?
newdata$weekday = weekdays(newdata$date)   <!-- add a new column containing day of week -->

newdata$weekday.type <- ifelse(newdata$weekday == "Saturday" | newdata$weekday == "Sunday", "Weekend", "Weekday")       <!-- add a new column containing either Weekday OR Weekend -->

newdata$weekday.type <- factor(newdata$weekday.type)    <!-- convert column to factor -->

groupedBy.Interval.WeekDay <- ddply(newdata, ~interval + weekday.type, summarise, mean = mean(steps))           <!-- make a new dataset grouping data by interval and weekday.type -->


groupedBy.Interval.WeekDay$interval <- as.numeric(as.character(groupedBy.Interval.WeekDay$interval))


library(lattice)
xyplot(mean ~ interval | weekday.type, groupedBy.Interval.WeekDay, type = "l", layout = c(1, 2), xlab = "Interval", ylab = "Number of steps")


We notice that mean stays the same and median increases by one in the above comparison.
However as we see from the time series the impact is very 'light'
