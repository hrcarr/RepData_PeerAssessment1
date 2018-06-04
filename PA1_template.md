---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

# R Markdown for Assignment 1
Clear memory and load necessary library files
rm(list=ls())
library(ggplot2)
install.packages("lattice")
require(lattice) 
library(lattice)



## Loading and preprocessing the data
setwd("/Users/hilarycarr/Desktop/R_directory/Reproducible Research")

```r
data <- read.csv("activity.csv")
data$date <- as.Date(data$date)
data$steps <- as.numeric(data$steps)
```
## What is mean total number of steps taken per day?
1. Calculate the total number of steps taken per day
2. Make a histogram of the total number of steps taken each day
3. Calculate and report the mean and median of the total number of steps taken per day


```r
steps_per_day <- aggregate(data$steps, by = list(data$date), sum, na.exclude=T)
names(steps_per_day) <- c("date", "steps")

hist(steps_per_day$steps, main = "Total Number of Steps per Day", xlab = "Number of Steps", col = "blue", breaks = 10)
```

![](PA1_template_files/figure-html/plots-1.png)<!-- -->

```r
print(mean_steps_per_day <- mean(steps_per_day$steps, na.rm=TRUE))
```

```
## [1] 10767.19
```

```r
print(med_steps_per_day <- median(steps_per_day$steps, na.rm=TRUE))
```

```
## [1] 10766
```

## What is the average daily activity pattern?
1. Make a time series plot (i.e.type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
steps_interval <- aggregate(data$steps, by =list(data$interval), mean, na.rm=TRUE)
names(steps_interval) <- c("interval", "steps")

plot(steps_interval$interval, steps_interval$steps, type="l", xlab = "5 min - interval", ylab = "Average steps", main = "Average Daily Activity Pattern", col = "red")
```

![](PA1_template_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

```r
print(intv <- steps_interval$interval[which.max(steps_interval$steps)])
```

```
## [1] 835
```

## Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
print(missingrows <- sum(is.na(data$steps)))
```

```
## [1] 2304
```

```r
data1 <- data
data1$steps[which(is.na(data1$steps))] <- mean(data1$step, na.rm = TRUE)
full_steps <- aggregate(data1$steps, by = list(data1$date), sum)
names(full_steps) <- c("date", "steps")

hist(full_steps$steps, main = "Total Number of Steps per Day \n (w/o NAs)", xlab = "Number of Steps", col = "blue", breaks = 10)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
summary(full_steps)
```

```
##       date                steps      
##  Min.   :2012-10-01   Min.   :   41  
##  1st Qu.:2012-10-16   1st Qu.: 9819  
##  Median :2012-10-31   Median :10766  
##  Mean   :2012-10-31   Mean   :10766  
##  3rd Qu.:2012-11-15   3rd Qu.:12811  
##  Max.   :2012-11-30   Max.   :21194
```
Conclusion: The values do not differ from the earlier estimates. By imputing the mean value for the missing data, the mean and median values in the full_steps data set remain virtually unchanged as a result.
## Are there differences in activity patterns between weekdays and weekends?
For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
2. Make a panel plot containing a time series plot (type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 



```r
data1$week <- "weekday"
data1$week[weekdays(as.Date(data1$date), abb=T) %in% c("Sat","Sun")] <- "weekend"
table(data1$week)
```

```
## 
## weekday weekend 
##   12960    4608
```

```r
full_inter <- a<- aggregate(steps ~ interval + week, data=data1, FUN="mean")
names(full_inter) <- c("interval", "week", "steps")
library(lattice)
xyplot(steps ~ interval | week, data=full_inter, type="l", layout = c(1, 2), 
       main="Average 5-minute Intervals: Weekdays vs. Weekends", ylab ="Number of Steps", xlab ="5-minute Intervals")
```

![](PA1_template_files/figure-html/week plot-1.png)<!-- -->
