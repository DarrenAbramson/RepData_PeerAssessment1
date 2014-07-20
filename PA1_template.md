---
title: "PA1_Template.md"
author: "Darren Abramson"
date: "July 19, 2014"
output: html_document
---

### Loading and preprocessing the data

The following code assigns the contents of the activity.zip file, activity.csv, to the R variable data.


```r
library(data.table)
temp = tempfile()
file.link("activity.zip", temp)
data <- read.table(unz(temp, "activity.csv"), header = TRUE, sep=",")
unlink(temp)
```

The following code performs preprocessing of the data, including converting the date column to the date format.


```r
data$date <- as.Date(data$date, format="%Y-%m-%d")
```

### Mean total number of steps taken per day

First, we examine the total number of steps taken each day.


```r
numStepsPerDay <- aggregate(steps ~ date, data, sum) 
hist(numStepsPerDay$steps, xlab = "Total number of steps in a day", main = "Histogram of steps in a day");
```

![plot of chunk histogram](figure/histogram.png) 

Now we examine mean and median of steps taken per today.

First, we calculate the mean.


```r
mean(numStepsPerDay$steps)
```

```
## [1] 10766
```

Now, we calculate the median.


```r
median(numStepsPerDay$steps)
```

```
## [1] 10765
```

## Average daily activity pattern

For the average daily activity pattern, we begin by aggregating steps across intervals. In particular, we take the mean of steps on each day within a given interval.


```r
avgStepsPerInterval <- aggregate(steps ~ interval, data, mean)
```

Now we plot the result.


```r
plot(avgStepsPerInterval, type="l", xlab="5 minute interval", ylab="Mean steps taken", main="Mean steps per 5 minute interval")
```

![plot of chunk daily pattern plot](figure/daily pattern plot.png) 

To find the interval with the highest mean number of steps, we select the row of our table of mean steps per interval with maximum of the steps column.

The interval value of this row is the desired maximum mean interval, confirmed by examining the plot above.


```r
avgStepsPerInterval[which.max(avgStepsPerInterval$steps),]
```

```
##     interval steps
## 104      835 206.2
```

Explicitly, the interval in question is returned by the following.


```r
avgStepsPerInterval[which.max(avgStepsPerInterval$steps),]$interval
```

```
## [1] 835
```

## Imputing missing values

First, we calculate the number of `NA` values. I do this by subsetting out the appropriate rows, and then counting them -- I have no doubt that there are many different ways this could be calculated.


```r
nrow(subset(data, is.na(data$steps)))
```

```
## [1] 2304
```

Now we replace NA values by the mean for that interval.


```r
naScrubbedData <- data
naScrubbedData$intervalAvg <- NA
matches <- match(naScrubbedData$interval, avgStepsPerInterval$interval)
naScrubbedData$intervalAvg <- avgStepsPerInterval[matches,2] 
naScrubbedData$steps[is.na(naScrubbedData$steps)] <- naScrubbedData$intervalAvg
```

```
## Warning: number of items to replace is not a multiple of replacement
## length
```

Now we produce a histogram, mean and median as above, but with the scrubbed data.


```r
scrubbedNumStepsPerDay <- aggregate(steps ~ date, naScrubbedData, sum) 
hist(scrubbedNumStepsPerDay$steps, xlab = "Total number of steps in a day", main = "Histogram of steps in a day");
```

![plot of chunk scrubbed histogram](figure/scrubbed histogram.png) 

Now we examine mean and median of steps taken per today.

First, we calculate the mean.


```r
mean(scrubbedNumStepsPerDay$steps)
```

```
## [1] 10766
```

Now, we calculate the median.


```r
median(scrubbedNumStepsPerDay$steps)
```

```
## [1] 10766
```

**Note**: the values we observe are very similar to the un-scrubbed data. This is likely due to the replacement of `NA` values with interval averages -- in effect, we are 'pushing' the data closer to the profile that we extracted in the un-scrubbed case. 


## Differences in activity patterns between weekdays and weekends

We copy the scrubbed data to a new table for the purpose of adding a column with factors for weekend/weekday.


```r
weekdayFactoredData <- naScrubbedData
weekdayFactoredData$isWeekend <- weekdays(weekdayFactoredData$date)
weekdayFactoredData$isWeekend <- weekdayFactoredData$isWeekend %in% c("Saturday", "Sunday")
weekdayFactoredData$dayType <- NA
weekdayFactoredData$dayType[weekdayFactoredData$isWeekend == TRUE] <- "weekend"
weekdayFactoredData$dayType[weekdayFactoredData$isWeekend == FALSE] <- "weekday"
```

Now we panel plot the activity data across weekends and weekdays.



```r
weekEnd <- subset(weekdayFactoredData, weekdayFactoredData$dayType == "weekend")

weekEndMean <- aggregate(steps ~ interval, weekEnd, mean)

weekDay <- subset(weekdayFactoredData, weekdayFactoredData$dayType == "weekday")

weekDayMean <- aggregate(steps ~ interval, weekDay, mean)

par(mfrow=c(2,1));

plot(weekDayMean, type="l", xlab="5 minute interval", ylab="Mean steps taken", main="Weekday steps per interval")
plot(weekEndMean, type="l", xlab="5 minute interval", ylab="Mean steps taken", main="Weekend steps per interval")
```

![plot of chunk plotting weekday weekend data](figure/plotting weekday weekend data.png) 
